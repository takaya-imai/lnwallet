package com.lightning.wallet.ln

import fr.acinq.bitcoin._
import com.lightning.wallet.ln.wire._
import com.lightning.wallet.ln.Scripts._
import com.lightning.wallet.ln.crypto.ShaChain._

import com.lightning.wallet.ln.crypto.Generators
import fr.acinq.bitcoin.Crypto.{Point, PublicKey, Scalar}
import scala.util.{Success, Try}


object Helpers { me =>
  val toMessage = UpdateFulfillHtlc(BinaryData.empty, 0L, _: BinaryData)
  def extractPreimages(tx: Transaction) = tx.txIn.map(_.witness.stack) collect {
    case Seq(BinaryData.empty, _, _, paymentPreimage, _) if paymentPreimage.size == 32 => toMessage(paymentPreimage) // htlc-success
    case Seq(_, paymentPreimage, _) if paymentPreimage.size == 32 => toMessage(paymentPreimage) // claim-htlc-success
  }

  def makeLocalTxs(commitTxNumber: Long, localParams: LocalParams,
                   remoteParams: AcceptChannel, commitmentInput: InputInfo,
                   localPerCommitmentPoint: Point, spec: CommitmentSpec) = {

    val remotePubkey = Generators.derivePubKey(remoteParams.paymentBasepoint, localPerCommitmentPoint)
    val localPubkey = Generators.derivePubKey(localParams.paymentKey.toPoint, localPerCommitmentPoint)
    val localDelayedPubkey = Generators.derivePubKey(localParams.delayedPaymentKey.toPoint, localPerCommitmentPoint)
    val localRevocationPubkey = Generators.revocationPubKey(remoteParams.revocationBasepoint, localPerCommitmentPoint)

    val commitTx = Scripts.makeCommitTx(commitmentInput, commitTxNumber, localParams.paymentKey.toPoint,
      remoteParams.paymentBasepoint, localParams.isFunder, LNParams.dustLimit, localPubkey, localRevocationPubkey,
      remoteParams.toSelfDelay, localDelayedPubkey, remotePubkey, spec)

    val (htlcTimeoutTxs, htlcSuccessTxs) = Scripts.makeHtlcTxs(commitTx.tx, LNParams.dustLimit,
      localRevocationPubkey, remoteParams.toSelfDelay, localPubkey, localDelayedPubkey, remotePubkey, spec)

    (commitTx, htlcTimeoutTxs, htlcSuccessTxs)
  }

  def makeRemoteTxs(commitTxNumber: Long, localParams: LocalParams,
                    remoteParams: AcceptChannel, commitmentInput: InputInfo,
                    remotePerCommitmentPoint: Point, spec: CommitmentSpec) = {

    val localPubkey = Generators.derivePubKey(localParams.paymentKey.toPoint, remotePerCommitmentPoint)
    val remotePubkey = Generators.derivePubKey(remoteParams.paymentBasepoint, remotePerCommitmentPoint)
    val remoteDelayedPubkey = Generators.derivePubKey(remoteParams.delayedPaymentBasepoint, remotePerCommitmentPoint)
    val remoteRevocationPubkey = Generators.revocationPubKey(localParams.revocationSecret.toPoint, remotePerCommitmentPoint)
    val commitTx = Scripts.makeCommitTx(commitmentInput, commitTxNumber, remoteParams.paymentBasepoint, localParams.paymentKey.toPoint,
      !localParams.isFunder, Satoshi(remoteParams.dustLimitSatoshis), remotePubkey, remoteRevocationPubkey, localParams.toSelfDelay,
      remoteDelayedPubkey, localPubkey, spec)

    val (htlcTimeoutTxs, htlcSuccessTxs) = Scripts.makeHtlcTxs(commitTx.tx, Satoshi(remoteParams.dustLimitSatoshis),
      remoteRevocationPubkey, localParams.toSelfDelay, remotePubkey, remoteDelayedPubkey, localPubkey, spec)

    (commitTx, htlcTimeoutTxs, htlcSuccessTxs, remotePubkey, remoteRevocationPubkey)
  }

  object Closing {
    type SuccessAndClaim = (HtlcSuccessTx, ClaimDelayedOutputTx)
    type TimeoutAndClaim = (HtlcTimeoutTx, ClaimDelayedOutputTx)

    def isValidFinalScriptPubkey(raw: BinaryData) = Try(Script parse raw) match {
      case Success(OP_DUP :: OP_HASH160 :: OP_PUSHDATA(pkh, _) :: OP_EQUALVERIFY :: OP_CHECKSIG :: Nil) => pkh.data.size == 20
      case Success(OP_HASH160 :: OP_PUSHDATA(scriptHash, _) :: OP_EQUAL :: Nil) => scriptHash.data.size == 20
      case Success(OP_0 :: OP_PUSHDATA(pubkeyHash, _) :: Nil) if pubkeyHash.length == 20 => true
      case Success(OP_0 :: OP_PUSHDATA(scriptHash, _) :: Nil) if scriptHash.length == 32 => true
      case _ => false
    }

    def makeFirstClosing(commitments: Commitments, localScriptPubkey: BinaryData,
                         remoteScriptPubkey: BinaryData, rate: Long): ClosingSigned = {

      // This is just to estimate the weight, it depends on size of the pubkey scripts
      val dummy: ClosingTx = Scripts.addSigs(makeFunderClosingTx(commitments.commitInput, localScriptPubkey,
        remoteScriptPubkey, dustLimit = Satoshi(0), closingFee = Satoshi(0), spec = commitments.localCommit.spec),
        commitments.localParams.fundingPrivKey.publicKey, commitments.remoteParams.fundingPubkey, "aa" * 71, "bb" * 71)

      val closingWeight = Transaction.weight(dummy.tx)
      val closingFee = Scripts.weight2fee(feeratePerKw = rate, closingWeight)
      val _ \ msg = makeClosing(commitments, localScriptPubkey, remoteScriptPubkey, closingFee)
      msg
    }

    def makeClosing(commitments: Commitments, localScriptPubkey: BinaryData,
                    remoteScriptPubkey: BinaryData, closingFee: Satoshi) = {

      require(isValidFinalScriptPubkey(localScriptPubkey), "Invalid localScriptPubkey")
      require(isValidFinalScriptPubkey(remoteScriptPubkey), "Invalid remoteScriptPubkey")

      val dustLimitSat = math.max(LNParams.dustLimit.amount, commitments.remoteParams.dustLimitSatoshis)
      val closingTx = makeFunderClosingTx(commitments.commitInput, localScriptPubkey, remoteScriptPubkey,
        Satoshi(dustLimitSat), closingFee, commitments.localCommit.spec)

      val localClosingSig = Scripts.sign(closingTx, commitments.localParams.fundingPrivKey)
      val closingSigned = ClosingSigned(commitments.channelId, closingFee.amount, localClosingSig)
      (closingTx, closingSigned)
    }

    def makeFunderClosingTx(commitTxInput: InputInfo, localScriptPubKey: BinaryData, remoteScriptPubKey: BinaryData,
                            dustLimit: Satoshi, closingFee: Satoshi, spec: CommitmentSpec): ClosingTx = {

      require(spec.htlcs.isEmpty, "No HTLCs allowed")
      val toRemoteAmount = Satoshi(spec.toRemoteMsat / 1000L)
      val toLocalAmount = Satoshi(spec.toLocalMsat / 1000L) - closingFee
      val toLocalOutput = if (toLocalAmount >= dustLimit) TxOut(toLocalAmount, localScriptPubKey) :: Nil else Nil
      val toRemoteOutput = if (toRemoteAmount >= dustLimit) TxOut(toRemoteAmount, remoteScriptPubKey) :: Nil else Nil
      val input = TxIn(commitTxInput.outPoint, Array.emptyByteArray, sequence = 0xffffffffL) :: Nil
      val tx = Transaction(version = 2, input, toLocalOutput ++ toRemoteOutput, lockTime = 0)
      ClosingTx(commitTxInput, LexicographicalOrdering sort tx)
    }

    def checkClosingSignature(commitments: Commitments, localScriptPubkey: BinaryData, remoteScriptPubkey: BinaryData,
                              remoteClosingFee: Satoshi, remoteClosingSig: BinaryData): Option[TransactionWithInputInfo] = {

      val (closingTx, closingSigned) = makeClosing(commitments, localScriptPubkey, remoteScriptPubkey, remoteClosingFee)
      Scripts checkSpendable Scripts.addSigs(closingTx, commitments.localParams.fundingPrivKey.publicKey,
        commitments.remoteParams.fundingPubkey, closingSigned.signature, remoteClosingSig)
    }

    def nextClosingFee(localClosingFee: Satoshi, remoteClosingFee: Satoshi): Satoshi =
      (localClosingFee + remoteClosingFee) / 4 * 2

    def claimCurrentLocalCommitTxOutputs(commitments: Commitments, tx: Transaction, bag: PaymentInfoBag) = {
      val localPerCommitmentPoint = Generators.perCommitPoint(commitments.localParams.shaSeed, commitments.localCommit.index.toInt)
      val localRevocationPubkey = Generators.revocationPubKey(commitments.remoteParams.revocationBasepoint, localPerCommitmentPoint)
      val localDelayedPrivkey = Generators.derivePrivKey(commitments.localParams.delayedPaymentKey, localPerCommitmentPoint)

      def makeClaimDelayedOutput(tx: Transaction): ClaimDelayedOutputTx = {
        val claimDelayed = Scripts.makeClaimDelayedOutputTx(tx, localRevocationPubkey, commitments.localParams.toSelfDelay,
          localDelayedPrivkey.publicKey, commitments.localParams.defaultFinalScriptPubKey, LNParams.broadcaster.feeRatePerKw)

        val sig = Scripts.sign(claimDelayed, localDelayedPrivkey)
        Scripts.addSigs(claimDelayed, sig)
      }

      val allSuccessTxs = for {
        HtlcTxAndSigs(info: HtlcSuccessTx, localSig, remoteSig) <- commitments.localCommit.htlcTxsAndSigs
        IncomingPayment(_, preimage, _, _, _) <- bag.getPaymentInfo(info.add.paymentHash).toOption
        success: HtlcSuccessTx = Scripts.addSigs(info, localSig, remoteSig, preimage)
        delayedClaim <- Scripts checkSpendable makeClaimDelayedOutput(success.tx)
        if delayedClaim.tx.txOut.head.amount >= LNParams.dustLimit
      } yield success -> delayedClaim

      val allTimeoutTxs = for {
        HtlcTxAndSigs(info: HtlcTimeoutTx, localSig, remoteSig) <- commitments.localCommit.htlcTxsAndSigs
        timeout: HtlcTimeoutTx = Scripts.addSigs(htlcTimeoutTx = info, localSig, remoteSig)
        delayedClaim <- Scripts checkSpendable makeClaimDelayedOutput(timeout.tx)
        if delayedClaim.tx.txOut.head.amount >= LNParams.dustLimit
      } yield timeout -> delayedClaim

      val claimMainDelayedTx = Scripts checkSpendable makeClaimDelayedOutput(tx)
      LocalCommitPublished(claimMainDelayedTx.toList, allSuccessTxs, allTimeoutTxs, commitTx = tx)
    }

    def claimRemoteCommitTxOutputs(commitments: Commitments, remoteCommit: RemoteCommit,
                                   bag: PaymentInfoBag): RemoteCommitPublished = {

      val localPrivkey = Generators.derivePrivKey(commitments.localParams.paymentKey, remoteCommit.remotePerCommitmentPoint)
      val (remoteCommitTx, timeoutTxs, successTxs, remotePubkey, remoteRevPubkey) = makeRemoteTxs(remoteCommit.index, commitments.localParams,
        commitments.remoteParams, commitments.commitInput, remoteCommit.remotePerCommitmentPoint, remoteCommit.spec)

      val claimSuccessTxs = for {
        HtlcSuccessTx(_, _, add) <- successTxs
        IncomingPayment(_, preimage, _, _, _) <- bag.getPaymentInfo(add.paymentHash).toOption
        claimHtlcSuccessTx = Scripts.makeClaimHtlcSuccessTx(remoteCommitTx.tx, localPrivkey.publicKey, remotePubkey,
          remoteRevPubkey, commitments.localParams.defaultFinalScriptPubKey, add, LNParams.broadcaster.feeRatePerKw)

        sig = Scripts.sign(claimHtlcSuccessTx, localPrivkey)
        signed = Scripts.addSigs(claimHtlcSuccessTx, sig, preimage)
        claimSuccess <- Scripts checkSpendable signed
      } yield claimSuccess

      val claimTimeoutTxs = for {
        HtlcTimeoutTx(_, _, add) <- timeoutTxs
        claimHtlcTimeoutTx = Scripts.makeClaimHtlcTimeoutTx(remoteCommitTx.tx, localPrivkey.publicKey, remotePubkey,
          remoteRevPubkey, commitments.localParams.defaultFinalScriptPubKey, add, LNParams.broadcaster.feeRatePerKw)

        sig = Scripts.sign(claimHtlcTimeoutTx, localPrivkey)
        signed = Scripts.addSigs(claimHtlcTimeoutTx, sig)
        claimTimeout <- Scripts checkSpendable signed
      } yield claimTimeout

      val claimMainTx = Scripts checkSpendable {
        val txWithInputInfo = Scripts.makeClaimP2WPKHOutputTx(remoteCommitTx.tx, localPrivkey.publicKey,
          commitments.localParams.defaultFinalScriptPubKey, LNParams.broadcaster.feeRatePerKw)

        val sig = Scripts.sign(txWithInputInfo, localPrivkey)
        Scripts.addSigs(txWithInputInfo, localPrivkey.publicKey, sig)
      }

      RemoteCommitPublished(claimMainTx.toList, claimSuccessTxs.toList,
        claimTimeoutTxs.toList, commitTx = remoteCommitTx.tx)
    }

    def claimRevokedRemoteCommitTxOutputs(commitments: Commitments, tx: Transaction) = {
      val txNumber = Scripts.obscuredCommitTxNumber(number = Scripts.decodeTxNumber(tx.txIn.head.sequence, tx.lockTime),
        !commitments.localParams.isFunder, commitments.remoteParams.paymentBasepoint, commitments.localParams.paymentKey.toPoint)

      val index = moves(largestTxIndex - txNumber)
      val hashes = commitments.remotePerCommitmentSecrets.hashes

      getHash(hashes, index) map { remotePerCommitmentSecret =>
        val remotePerCommitmentSecretScalar = Scalar(remotePerCommitmentSecret)
        val remotePerCommitmentPoint = remotePerCommitmentSecretScalar.toPoint

        val remoteDelayedPubkey = Generators.derivePubKey(commitments.remoteParams.delayedPaymentBasepoint, remotePerCommitmentPoint)
        val remoteRevocationPrivkey = Generators.revocationPrivKey(commitments.localParams.revocationSecret, remotePerCommitmentSecretScalar)
        val localPrivkey = Generators.derivePrivKey(commitments.localParams.paymentKey, remotePerCommitmentPoint)

        val claimMainTx = Scripts checkSpendable {
          val claimMain = Scripts.makeClaimP2WPKHOutputTx(tx, localPrivkey.publicKey,
            commitments.localParams.defaultFinalScriptPubKey, LNParams.broadcaster.feeRatePerKw)

          val sig = Scripts.sign(claimMain, localPrivkey)
          Scripts.addSigs(claimMain, localPrivkey.publicKey, sig)
        }

        val claimPenaltyTx = Scripts checkSpendable {
          val txinfo = Scripts.makeMainPenaltyTx(tx, remoteRevocationPrivkey.publicKey,
            commitments.localParams.defaultFinalScriptPubKey, commitments.remoteParams.toSelfDelay,
            remoteDelayedPubkey, LNParams.broadcaster.feeRatePerKw)

          val sig = Scripts.sign(txinfo, remoteRevocationPrivkey)
          Scripts.addSigs(txinfo, sig)
        }

        RevokedCommitPublished(claimMainTx.toList,
          claimPenaltyTx.toList, commitTx = tx)
      }
    }
  }

  object Funding {
    def makeFundingInputInfo(fundingTxHash: BinaryData, fundingTxOutputIndex: Int,
                             fundingSatoshis: Satoshi, fundingPubkey1: PublicKey,
                             fundingPubkey2: PublicKey): InputInfo = {

      val multisig = Scripts.multiSig2of2(fundingPubkey1, fundingPubkey2)
      val fundingTxOut = TxOut(fundingSatoshis, Script pay2wsh multisig)
      val outPoint = OutPoint(fundingTxHash, fundingTxOutputIndex)
      InputInfo(outPoint, fundingTxOut, Script write multisig)
    }

    // Assuming we are always a funder
    def makeFirstFunderCommitTxs(cmd: CMDOpenChannel, remoteParams: AcceptChannel,
                                 fundingTxHash: BinaryData, fundingTxOutputIndex: Int,
                                 remoteFirstPoint: Point) = {

      val toLocalMsat = cmd.fundingAmountSat * 1000L - cmd.pushMsat
      val commitmentInput = makeFundingInputInfo(fundingTxHash, fundingTxOutputIndex,
        Satoshi(cmd.fundingAmountSat), cmd.localParams.fundingPrivKey.publicKey,
        remoteParams.fundingPubkey)

      val localPerCommitmentPoint = Generators.perCommitPoint(cmd.localParams.shaSeed, 0)
      val localSpec = CommitmentSpec(Set.empty, Set.empty, Set.empty, cmd.initialFeeratePerKw, toLocalMsat, cmd.pushMsat)
      val remoteSpec = CommitmentSpec(Set.empty, Set.empty, Set.empty, cmd.initialFeeratePerKw, cmd.pushMsat, toLocalMsat)
      val (localCommitTx, _, _) = makeLocalTxs(0, cmd.localParams, remoteParams, commitmentInput, localPerCommitmentPoint, localSpec)
      val (remoteCommitTx, _, _, _, _) = makeRemoteTxs(0, cmd.localParams, remoteParams, commitmentInput, remoteFirstPoint, remoteSpec)
      (localSpec, localCommitTx, remoteSpec, remoteCommitTx)
    }
  }
}