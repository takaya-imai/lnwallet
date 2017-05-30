package com.lightning.wallet.ln

import fr.acinq.bitcoin._
import com.lightning.wallet.ln.wire._
import com.lightning.wallet.ln.Scripts._

import scala.util.{Success, Try}
import fr.acinq.bitcoin.Crypto.{Point, PublicKey}
import com.lightning.wallet.ln.crypto.Generators
import com.lightning.wallet.ln.MSat.satFactor


object Helpers { me =>
  def extractOutgoingMessages(previousData: Any, nextData: Any) = (previousData, nextData) match {
    case (c0: NormalData, c1: NormalData) => c1.commitments.unackedMessages diff c0.commitments.unackedMessages

    // When in NORMAL state we launch a closed app again
    case (c0: NormalData, _) => c0.commitments.unackedMessages
    case (_, c1: NormalData) => c1.commitments.unackedMessages

    // When initializing a new channel before a funding tx is broadcasted
    case (_, waitAccept: WaitAcceptData) => Vector(waitAccept.openChannelMessage)
    case (_, waitSigned: WaitFundingSignedData) => Vector(waitSigned.fundingCreatedMessage)

    // When funding tx has already been broadcasted and we launch an app again
    case (waitConfirmed: WaitFundingConfirmedData, _) => waitConfirmed.our.toVector
    case (_, waitConfirmed: WaitFundingConfirmedData) => waitConfirmed.our.toVector
    case _ => Vector.empty
  }

  def extractPreimages(tx: Transaction): Seq[BinaryData] = tx.txIn.map(_.witness.stack) flatMap {
    case Seq(BinaryData.empty, _, _, BinaryData.empty, script) => Some(script.slice(109, 109 + 20): BinaryData) // htlc-timeout
    case Seq(_, BinaryData.empty, script) => Some(script.slice(109, 69 + 20): BinaryData) // claim-htlc-timeout
    case Seq(BinaryData.empty, _, _, preimg, _) if preimg.length == 32 => Some(preimg) // htlc-success
    case Seq(_, preimg, _) if preimg.length == 32 => Some(preimg) // claim-htlc-success
    case _ => None
  }

  def makeLocalTxs(commitTxNumber: Long, localParams: LocalParams,
                   remoteParams: RemoteParams, commitmentInput: InputInfo,
                   localPerCommitmentPoint: Point, spec: CommitmentSpec) = {

    val remotePubkey = Generators.derivePubKey(remoteParams.paymentBasepoint, localPerCommitmentPoint)
    val localPubkey = Generators.derivePubKey(localParams.paymentKey.toPoint, localPerCommitmentPoint)
    val localDelayedPubkey = Generators.derivePubKey(localParams.delayedPaymentKey.toPoint, localPerCommitmentPoint)
    val localRevocationPubkey = Generators.revocationPubKey(remoteParams.revocationBasepoint, localPerCommitmentPoint)

    val commitTx = Scripts.makeCommitTx(commitmentInput, commitTxNumber, localParams.paymentKey.toPoint,
      remoteParams.paymentBasepoint, localParams.isFunder, Satoshi(localParams.dustLimitSatoshis), localPubkey,
      localRevocationPubkey, remoteParams.toSelfDelay, localDelayedPubkey, remotePubkey, spec)

    val (htlcTimeoutTxs, htlcSuccessTxs) = Scripts.makeHtlcTxs(commitTx.tx, Satoshi(localParams.dustLimitSatoshis),
      localRevocationPubkey, remoteParams.toSelfDelay, localPubkey, localDelayedPubkey, remotePubkey, spec)

    (commitTx, htlcTimeoutTxs, htlcSuccessTxs)
  }

  def makeRemoteTxs(commitTxNumber: Long, localParams: LocalParams,
                    remoteParams: RemoteParams, commitmentInput: InputInfo,
                    remotePerCommitmentPoint: Point, spec: CommitmentSpec) = {

    val localPubkey = Generators.derivePubKey(localParams.paymentKey.toPoint, remotePerCommitmentPoint)
    val remotePubkey = Generators.derivePubKey(remoteParams.paymentBasepoint, remotePerCommitmentPoint)
    val remoteDelayedPubkey = Generators.derivePubKey(remoteParams.delayedPaymentBasepoint, remotePerCommitmentPoint)
    val remoteRevocationPubkey = Generators.revocationPubKey(localParams.revocationSecret.toPoint, remotePerCommitmentPoint)

    val commitTx = Scripts.makeCommitTx(commitmentInput, commitTxNumber, remoteParams.paymentBasepoint,
      localParams.paymentKey.toPoint, !localParams.isFunder, Satoshi(remoteParams.dustLimitSatoshis), remotePubkey,
      remoteRevocationPubkey, localParams.toSelfDelay, remoteDelayedPubkey, localPubkey, spec)

    val (htlcTimeoutTxs, htlcSuccessTxs) = Scripts.makeHtlcTxs(commitTx.tx, Satoshi(localParams.dustLimitSatoshis),
      remoteRevocationPubkey, localParams.toSelfDelay, remotePubkey, remoteDelayedPubkey, localPubkey, spec)

    (commitTx, htlcTimeoutTxs, htlcSuccessTxs)
  }

  object Closing {
    def makeTx(descr: String) = (attempt: TransactionWithInputInfo) => {
      val result = Try(attempt).filter(Scripts.checkSpendable(_).isSuccess)
      for (error <- result.failed) Tools log s"$descr generation error: $error"
      result.map(_.tx).toOption
    }

    def isValidFinalScriptPubkey(scriptPubKey: BinaryData): Boolean = Try(Script parse scriptPubKey) match {
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
        commitments.localParams.fundingPrivKey.publicKey, commitments.remoteParams.fundingPubKey, "aa" * 71, "bb" * 71)

      val closingWeight = Transaction.weight(dummy.tx)
      val closingFee = Scripts.weight2fee(feeratePerKw = rate, closingWeight)
      val (_, msg) = makeClosing(commitments, localScriptPubkey, remoteScriptPubkey, closingFee)
      msg
    }

    def makeClosing(commitments: Commitments, localScriptPubkey: BinaryData,
                    remoteScriptPubkey: BinaryData, closingFee: Satoshi) = {

      require(isValidFinalScriptPubkey(localScriptPubkey), "Invalid localScriptPubkey")
      require(isValidFinalScriptPubkey(remoteScriptPubkey), "Invalid remoteScriptPubkey")

      val dustLimitSat = math.max(commitments.localParams.dustLimitSatoshis, commitments.remoteParams.dustLimitSatoshis)
      val closingTx = makeFunderClosingTx(commitments.commitInput, localScriptPubkey, remoteScriptPubkey,
        Satoshi(dustLimitSat), closingFee, commitments.localCommit.spec)

      val localClosingSig = Scripts.sign(closingTx, commitments.localParams.fundingPrivKey)
      val closingSigned = ClosingSigned(commitments.channelId, closingFee.amount, localClosingSig)
      (closingTx, closingSigned)
    }

    def makeFunderClosingTx(commitTxInput: InputInfo, localScriptPubKey: BinaryData, remoteScriptPubKey: BinaryData,
                            dustLimit: Satoshi, closingFee: Satoshi, spec: CommitmentSpec): ClosingTx = {

      require(spec.htlcs.isEmpty, "No HTLCs allowed")
      val toRemoteAmount = Satoshi(spec.toRemoteMsat * satFactor)
      val toLocalAmount = Satoshi(spec.toLocalMsat * satFactor) - closingFee
      val toLocalOutput = if (toLocalAmount >= dustLimit) TxOut(toLocalAmount, localScriptPubKey) :: Nil else Nil
      val toRemoteOutput = if (toRemoteAmount >= dustLimit) TxOut(toRemoteAmount, remoteScriptPubKey) :: Nil else Nil
      val input = TxIn(commitTxInput.outPoint, Array.emptyByteArray, sequence = 0xffffffffL) :: Nil
      val tx = Transaction(version = 2, input, toLocalOutput ++ toRemoteOutput, lockTime = 0)
      ClosingTx(commitTxInput, LexicographicalOrdering sort tx)
    }

    def checkClosingSignature(commitments: Commitments, localScriptPubkey: BinaryData,
                              remoteScriptPubkey: BinaryData, remoteClosingFee: Satoshi,
                              remoteClosingSig: BinaryData): (Boolean, Transaction) = {

      val (closingTx, closingSigned) = makeClosing(commitments, localScriptPubkey, remoteScriptPubkey, remoteClosingFee)
      val signedClosingTx: ClosingTx = Scripts.addSigs(closingTx, commitments.localParams.fundingPrivKey.publicKey,
        commitments.remoteParams.fundingPubKey, closingSigned.signature, remoteClosingSig)

      val check = Scripts checkSpendable signedClosingTx
      (check.isSuccess, signedClosingTx.tx)
    }

    def nextClosingFee(localClosingFee: Satoshi, remoteClosingFee: Satoshi) = {
      val resultingClosingFee: Satoshi = (localClosingFee + remoteClosingFee) / 4 * 2
      if (resultingClosingFee == localClosingFee) resultingClosingFee + Satoshi(2)
      else resultingClosingFee
    }

    def claimCurrentLocalCommitTxOutputs(commitments: Commitments, tx: Transaction, bag: PaymentSpecBag) = {
      require(commitments.localCommit.publishableTxs.commitTx.tx.txid == tx.txid, "Txid mismatch, it's not current commit")
      val localPerCommitmentPoint = Generators.perCommitPoint(commitments.localParams.shaSeed, commitments.localCommit.index.toInt)
      val localRevocationPubkey = Generators.revocationPubKey(commitments.remoteParams.revocationBasepoint, localPerCommitmentPoint)
      val localDelayedPrivkey = Generators.derivePrivKey(commitments.localParams.delayedPaymentKey, localPerCommitmentPoint)
      val localTxs = commitments.localCommit.publishableTxs.htlcTxsAndSigs

      def makeClaimDelayedOutput(txn: Transaction, descr: String): Option[Transaction] = makeTx(descr) apply {
        val claimDelayed = Scripts.makeClaimDelayedOutputTx(txn, localRevocationPubkey, commitments.localParams.toSelfDelay,
          localDelayedPrivkey.publicKey, commitments.localParams.defaultFinalScriptPubKey, commitments.localCommit.spec.feeratePerKw)

        val sig = Scripts.sign(claimDelayed, localDelayedPrivkey)
        Scripts.addSigs(claimDelayed, sig)
      }

      val allSuccessTxs = for {
        HtlcTxAndSigs(info: HtlcSuccessTx, localSig, remoteSig) <- localTxs
        IncomingPaymentSpec(_, preimage, _) <- bag.getInfoByHash(info.paymentHash).map(_.spec).toOption
        success <- makeTx("htlc-success") apply Scripts.addSigs(info, localSig, remoteSig, preimage)
        successClaim <- makeClaimDelayedOutput(success, "htlc-success-claim-delayed")
      } yield success -> successClaim

      val allTimeoutTxs = for {
        HtlcTxAndSigs(info: HtlcTimeoutTx, localSig, remoteSig) <- localTxs
        timeout <- makeTx("htlc-timeout") apply Scripts.addSigs(info, localSig, remoteSig)
        timeoutClaim <- makeClaimDelayedOutput(timeout, "htlc-timeout-claim-delayed")
      } yield timeout -> timeoutClaim

      val (successTxs, claimSuccessTxs) = allSuccessTxs.unzip
      val (timeoutTxs, claimTimeoutTxs) = allTimeoutTxs.unzip

      LocalCommitPublished(makeClaimDelayedOutput(tx, "main-claim-delayed").toList,
        successTxs, htlcTimeoutTxs = timeoutTxs, claimHtlcSuccessTxs = claimSuccessTxs,
        claimHtlcTimeoutTxs = claimTimeoutTxs, commitTx = tx)
    }

    def claimRemoteCommitTxOutputs(commitments: Commitments, remoteCommit: RemoteCommit,
                                   tx: Transaction, bag: PaymentSpecBag): RemoteCommitPublished = {

      val (remoteCommitTx, _, _) = makeRemoteTxs(remoteCommit.index, commitments.localParams,
        commitments.remoteParams, commitments.commitInput, remoteCommit.remotePerCommitmentPoint,
        remoteCommit.spec)

      require(remoteCommitTx.tx.txid == tx.txid, "Txid mismatch, cannot recompute the current remote commit")
      val localPrivkey = Generators.derivePrivKey(commitments.localParams.paymentKey, remoteCommit.remotePerCommitmentPoint)
      val remotePubkey = Generators.derivePubKey(commitments.remoteParams.paymentBasepoint, remoteCommit.remotePerCommitmentPoint)
      val remoteRevocationPubkey = Generators.revocationPubKey(commitments.localParams.revocationSecret.toPoint,
        remoteCommit.remotePerCommitmentPoint)

      // Remember we are looking at the remote commitment so incoming=true
      // for them is really incoming=false for us and vice versa

      val claimSuccessTxs = for {
        Htlc(false, add) <- remoteCommit.spec.htlcs
        IncomingPaymentSpec(_, preimage, _) <- bag.getInfoByHash(add.paymentHash).map(_.spec).toOption
        info: ClaimHtlcSuccessTx = Scripts.makeClaimHtlcSuccessTx(remoteCommitTx.tx, localPrivkey.publicKey, remotePubkey,
          remoteRevocationPubkey, commitments.localParams.defaultFinalScriptPubKey, add, remoteCommit.spec.feeratePerKw)

        sig = Scripts.sign(info, localPrivkey)
        infoWithSig: ClaimHtlcSuccessTx = Scripts.addSigs(info, sig, preimage)
        claimSuccess <- makeTx("claim-htlc-success") apply infoWithSig
      } yield claimSuccess

      val claimTimeoutTxs = for {
        Htlc(true, add) <- remoteCommit.spec.htlcs
        info: ClaimHtlcTimeoutTx = Scripts.makeClaimHtlcTimeoutTx(remoteCommitTx.tx, localPrivkey.publicKey, remotePubkey,
          remoteRevocationPubkey, commitments.localParams.defaultFinalScriptPubKey, add, remoteCommit.spec.feeratePerKw)

        sig = Scripts.sign(info, localPrivkey)
        infoWithSig: ClaimHtlcTimeoutTx = Scripts.addSigs(info, sig)
        claimTimeout <- makeTx("claim-p2wpkh-output") apply infoWithSig
      } yield claimTimeout

      val mainTx = makeTx("claim-p2wpkh-output") apply {
        val info: ClaimP2WPKHOutputTx = Scripts.makeClaimP2WPKHOutputTx(tx, localPrivkey.publicKey,
          commitments.localParams.defaultFinalScriptPubKey, remoteCommit.spec.feeratePerKw)

        val sig = Scripts.sign(info, localPrivkey)
        Scripts.addSigs(info, localPrivkey.publicKey, sig)
      }

      RemoteCommitPublished(mainTx.toList,
        claimHtlcSuccessTxs = claimSuccessTxs.toSeq,
        claimHtlcTimeoutTxs = claimTimeoutTxs.toSeq,
        commitTx = tx)
    }

    def claimRevokedRemoteCommitTxOutputs(commitments: Commitments, tx: Transaction): Option[RevokedCommitPublished] = ???
  }

  object Funding {
    def makeFundingInputInfo(fundingTxId: BinaryData, fundingTxOutputIndex: Int,
                             fundingSatoshis: Satoshi, fundingPubkey1: PublicKey,
                             fundingPubkey2: PublicKey): InputInfo = {

      val fundingScript = Scripts.multiSig2of2(fundingPubkey1, fundingPubkey2)
      val fundingTxOut = TxOut(fundingSatoshis, Script pay2wsh fundingScript)
      val outPoint = OutPoint(fundingTxId, index = fundingTxOutputIndex)
      InputInfo(outPoint, fundingTxOut, Script write fundingScript)
    }

    // Assuming we are always a funder
    def makeFirstFunderCommitTxs(cmd: CMDOpenChannel, remoteParams: RemoteParams,
                                 fundingTxHash: BinaryData, fundingTxOutputIndex: Int,
                                 remoteFirstPerCommitmentPoint: Point) = {

      val toLocalMsat = cmd.fundingAmountSat * satFactor - cmd.pushMsat
      val commitmentInput = makeFundingInputInfo(fundingTxHash, fundingTxOutputIndex,
        Satoshi(cmd.fundingAmountSat), cmd.localParams.fundingPrivKey.publicKey,
        remoteParams.fundingPubKey)

      val localPerCommitmentPoint = Generators.perCommitPoint(cmd.localParams.shaSeed, 0)
      val localSpec = CommitmentSpec(feeratePerKw = cmd.initialFeeratePerKw, toLocalMsat = toLocalMsat, toRemoteMsat = cmd.pushMsat)
      val remoteSpec = CommitmentSpec(feeratePerKw = cmd.initialFeeratePerKw, toLocalMsat = cmd.pushMsat, toRemoteMsat = toLocalMsat)
      val (localCommitTx, _, _) = makeLocalTxs(0, cmd.localParams, remoteParams, commitmentInput, localPerCommitmentPoint, localSpec)
      val (remoteCommitTx, _, _) = makeRemoteTxs(0, cmd.localParams, remoteParams, commitmentInput, remoteFirstPerCommitmentPoint, remoteSpec)
      (localSpec, localCommitTx, remoteSpec, remoteCommitTx)
    }
  }
}