package com.lightning.wallet;

import org.bitcoinj.core.*;
import com.google.common.util.concurrent.AbstractIdleService;
import com.google.common.util.concurrent.ListenableFuture;
import org.bitcoinj.core.listeners.PeerDataEventListener;
import com.google.common.util.concurrent.FutureCallback;
import com.google.common.util.concurrent.Futures;
import org.bitcoinj.store.SPVBlockStore;
import org.bitcoinj.wallet.Wallet;
import javax.annotation.Nullable;
import java.util.List;


public abstract class AbstractKit extends AbstractIdleService {
    public static final String CURRENCY = "fiatCurrencyOfChoice";

    // Bitcoin wallet core pieces
    public volatile BlockChain blockChain;
    public volatile PeerGroup peerGroup;
    public volatile SPVBlockStore store;
    public volatile Wallet wallet;

    private class NonePeerDataListener implements PeerDataEventListener {
        public void onBlocksDownloaded(Peer peer, Block block, @Nullable FilteredBlock fb, int left) { /* none */ }
        public Message onPreMessageReceived(Peer peerSender, Message message) { return message; }
        public List<Message> getData(Peer peer, GetDataMessage msg) { return null; }
        public void onChainDownloadStarted(Peer peer, int left) { /* none */ }
    }

    private NonePeerDataListener noopPeerListener = new NonePeerDataListener();

    public void startDownload() {
        FutureCallback futureListener = new FutureCallback() {
            @Override public void onSuccess(@Nullable Object res) {
                peerGroup.startBlockChainDownload(noopPeerListener);
            }

            @Override public void onFailure(@Nullable Throwable err) {
                throw new RuntimeException(err);
            }
        };

        ListenableFuture future = peerGroup.startAsync();
        Futures.addCallback(future, futureListener);
    }
}