package com.lightning.wallet.ln

import fr.acinq.bitcoin._
import com.lightning.wallet.ln.Scripts._
import com.lightning.wallet.ln.Exceptions._

import scala.util.{Success, Try}
import fr.acinq.bitcoin.Crypto.{Point, PublicKey}

import com.lightning.wallet.ln.crypto.Sphinx.BinaryDataSeq
import com.lightning.wallet.ln.wire.ClosingSigned
import com.lightning.wallet.ln.crypto.Generators


object Helpers { me =>
  def validateParams(channelReserveSatoshis: Long, fundingSatoshis: Long): Unit = {
    val nope = channelReserveSatoshis.toDouble / fundingSatoshis > LNParams.maxReserveToFundingRatio
    if (nope) throw new RuntimeException(CHANNEL_RESERVE_TOO_HIGH)
  }

  def extractPreimages(tx: Transaction): BinaryDataSeq = tx.txIn.map(_.witness.stack) flatMap {
    case timeout @ Seq(BinaryData.empty, _, _, BinaryData.empty, script) => Some(script.slice(109, 109 + 20): BinaryData)
    case claimTimeout @ Seq(_, BinaryData.empty, script) => Some(script.slice(109, 109 + 20): BinaryData)
    case success @ Seq(BinaryData.empty, _, _, preimage, _) if preimage.length == 32 => Some(preimage)
    case claimSuccess @ Seq(_, preimage, _) if preimage.length == 32 => Some(preimage)
    case _ => None
  }

  def shouldUpdateFee(commitmentFeeratePerKw: Long, networkFeeratePerKw: Long): Boolean = {
    val feeRatio = (networkFeeratePerKw - commitmentFeeratePerKw) / commitmentFeeratePerKw.toDouble
    networkFeeratePerKw > 0 && Math.abs(feeRatio) > LNParams.updateFeeMinDiffRatio
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
      for (err <- result.failed) println(s"$descr generation error: $err")
      result.map(_.tx).toOption
    }

    def isValidFinalScriptPubkey(scriptPubKey: BinaryData): Boolean = Try(Script parse scriptPubKey) match {
      case Success(OP_DUP :: OP_HASH160 :: OP_PUSHDATA(pkh, _) :: OP_EQUALVERIFY :: OP_CHECKSIG :: Nil) => pkh.data.size == 20
      case Success(OP_HASH160 :: OP_PUSHDATA(scriptHash, _) :: OP_EQUAL :: Nil) => scriptHash.data.size == 20
      case Success(OP_0 :: OP_PUSHDATA(pubkeyHash, _) :: Nil) if pubkeyHash.length == 20 => true
      case Success(OP_0 :: OP_PUSHDATA(scriptHash, _) :: Nil) if scriptHash.length == 32 => true
      case _ => false
    }

    def makeFirstClosingTx(commitments: Commitments, localScriptPubkey: BinaryData,
                           remoteScriptPubkey: BinaryData, rate: Long): ClosingSigned = {

      // This is just to estimate the weight, it depends on size of the pubkey scripts
      val dummyClosingTx = Scripts.addSigs(Scripts.makeClosingTx(commitments.commitInput, localScriptPubkey,
        remoteScriptPubkey, commitments.localParams.isFunder, Satoshi(0), Satoshi(0), commitments.localCommit.spec),
        commitments.localParams.fundingPrivKey.publicKey, commitments.remoteParams.fundingPubKey, "aa" * 71, "bb" * 71)

      val closingWeight = Transaction.weight(dummyClosingTx.tx)
      val closingFee = Scripts.weight2fee(feeratePerKw = rate, closingWeight)
      val (_, cs) = makeClosingTx(commitments, localScriptPubkey, remoteScriptPubkey, closingFee)
      cs
    }

    def makeClosingTx(commitments: Commitments, localScriptPubkey: BinaryData,
                      remoteScriptPubkey: BinaryData, closingFee: Satoshi) = {

      require(isValidFinalScriptPubkey(localScriptPubkey), "Invalid localScriptPubkey")
      require(isValidFinalScriptPubkey(remoteScriptPubkey), "Invalid remoteScriptPubkey")

      val dustLimitSatoshis = math.max(commitments.localParams.dustLimitSatoshis, commitments.remoteParams.dustLimitSatoshis)
      val closingTx: ClosingTx = Scripts.makeClosingTx(commitments.commitInput, localScriptPubkey, remoteScriptPubkey,
        commitments.localParams.isFunder, Satoshi(dustLimitSatoshis), closingFee, commitments.localCommit.spec)

      val localClosingSig = Scripts.sign(closingTx, commitments.localParams.fundingPrivKey)
      val closingSigned = ClosingSigned(commitments.channelId, closingFee.amount, localClosingSig)
      (closingTx, closingSigned)
    }

    def checkClosingSignature(commitments: Commitments, localScriptPubkey: BinaryData,
                              remoteScriptPubkey: BinaryData, remoteClosingFee: Satoshi,
                              remoteClosingSig: BinaryData): Try[Transaction] = {

      val (closingTx, closingSigned) = makeClosingTx(commitments, localScriptPubkey, remoteScriptPubkey, remoteClosingFee)
      val signedClosingTx: ClosingTx = Scripts.addSigs(closingTx, commitments.localParams.fundingPrivKey.publicKey,
        commitments.remoteParams.fundingPubKey, closingSigned.signature, remoteClosingSig)

      // Map positive check result into transaction
      val check = Scripts checkSpendable signedClosingTx
      check.map(_ => signedClosingTx.tx)
    }

    def nextClosingFee(localClosingFee: Satoshi, remoteClosingFee: Satoshi) = {
      val resultingClosingFee: Satoshi = (localClosingFee + remoteClosingFee) / 4 * 2
      if (resultingClosingFee == localClosingFee) resultingClosingFee + Satoshi(2)
      else resultingClosingFee
    }

    def claimCurrentLocalCommitTxOutputs(commitments: Commitments, tx: Transaction, bag: InvoiceBag, rate: Long) = {
      require(commitments.localCommit.publishableTxs.commitTx.tx.txid == tx.txid, "Txid mismatch, it's not current commit")
      val localPerCommitmentPoint = Generators.perCommitPoint(commitments.localParams.shaSeed, commitments.localCommit.index.toInt)
      val localRevocationPubkey = Generators.revocationPubKey(commitments.remoteParams.revocationBasepoint, localPerCommitmentPoint)
      val localDelayedPrivkey = Generators.derivePrivKey(commitments.localParams.delayedPaymentKey, localPerCommitmentPoint)
      val localTxs = commitments.localCommit.publishableTxs.htlcTxsAndSigs

      def makeClaimDelayedOutput(txn: Transaction, descr: String) = makeTx(descr) apply {
        val claimDelayed = Scripts.makeClaimDelayedOutputTx(txn, localRevocationPubkey, commitments.localParams.toSelfDelay,
          localDelayedPrivkey.publicKey, commitments.localParams.defaultFinalScriptPubKey, feeratePerKw = rate)

        val sig = Scripts.sign(claimDelayed, localDelayedPrivkey)
        Scripts.addSigs(claimDelayed, sig)
      }

      val allSuccessTxs = for {
        HtlcTxAndSigs(info: HtlcSuccessTx, localSig, remoteSig) <- localTxs
        ExtendedInvoice(Some(preimage), _, _, _, _, _) <- bag getExtendedInvoice info.paymentHash
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

      LocalCommitPublished(claimMainDelayedOutputTx = makeClaimDelayedOutput(tx, "main-claim-delayed"),
        htlcSuccessTxs = successTxs, htlcTimeoutTxs = timeoutTxs, claimHtlcSuccessTxs = claimSuccessTxs,
        claimHtlcTimeoutTxs = claimTimeoutTxs, commitTx = tx)
    }

    def claimRemoteCommitTxOutputs(commitments: Commitments, remoteCommit: RemoteCommit,
                                   tx: Transaction, bag: InvoiceBag, rate: Long) = {

      val (remoteCommitTx, _, _) = makeRemoteTxs(remoteCommit.index, commitments.localParams,
        commitments.remoteParams, commitments.commitInput, remoteCommit.remotePerCommitmentPoint,
        remoteCommit.spec)

      require(remoteCommitTx.tx.txid == tx.txid, "Txid mismatch, cannot recompute the current remote commit tx")
      val localPrivkey = Generators.derivePrivKey(commitments.localParams.paymentKey, remoteCommit.remotePerCommitmentPoint)
      val remotePubkey = Generators.derivePubKey(commitments.remoteParams.paymentBasepoint, remoteCommit.remotePerCommitmentPoint)
      val remoteRevocationPubkey = Generators.revocationPubKey(commitments.localParams.revocationSecret.toPoint,
        remoteCommit.remotePerCommitmentPoint)

      // Remember we are looking at the remote commitment so incoming=true
      // for them is really incoming=false for us and vice versa

      val claimSuccessTxs = for {
        Htlc(false, add, _, _, _) <- commitments.remoteCommit.spec.htlcs
        ExtendedInvoice(Some(preimage), _, _, _, _, _) <- bag getExtendedInvoice add.paymentHash
        info = Scripts.makeClaimHtlcSuccessTx(remoteCommitTx.tx, localPrivkey.publicKey, remotePubkey,
          remoteRevocationPubkey, commitments.localParams.defaultFinalScriptPubKey, add, feeratePerKw = rate)

        sig = Scripts.sign(info, localPrivkey)
        infoWithSigs = Scripts.addSigs(claimHtlcSuccessTx = info, sig, preimage)
        tx <- makeTx("claim-htlc-success") apply infoWithSigs
      } yield tx

      val claimTimeoutTxs = for {
        Htlc(true, add, _, _, _) <- commitments.remoteCommit.spec.htlcs
        info = Scripts.makeClaimHtlcTimeoutTx(remoteCommitTx.tx, localPrivkey.publicKey, remotePubkey,
          remoteRevocationPubkey, commitments.localParams.defaultFinalScriptPubKey, add, feeratePerKw = rate)

        sig = Scripts.sign(info, localPrivkey)
        infoWithSigs = Scripts.addSigs(claimHtlcTimeoutTx = info, sig)
        tx <- makeTx("claim-p2wpkh-output") apply infoWithSigs
      } yield tx

      val mainTx = makeTx("claim-p2wpkh-output") apply {
        val info = Scripts.makeClaimP2WPKHOutputTx(tx, localPrivkey.publicKey,
          commitments.localParams.defaultFinalScriptPubKey, feeratePerKw = rate)

        val sig = Scripts.sign(info, localPrivkey)
        Scripts.addSigs(info, localPrivkey.publicKey, sig)
      }

      RemoteCommitPublished(claimMainOutputTx = mainTx,
        claimHtlcSuccessTxs = claimSuccessTxs.toSeq,
        claimHtlcTimeoutTxs = claimTimeoutTxs.toSeq,
        commitTx = tx)
    }
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
  }
}