---
layout: default
---

### [](#using-lightning-wallet)Using Lightning wallet

Once Bitcoin wallet is created you can also start using a Lightning wallet by opening a payment channel with a peer of your choice and using it to send payments across a Lightning network.

### [](#payment-channel)Payment channel

Technically speaking it is a multisig lockbox on a Bitcoin blockchain which is mutually controlled by you and a peer of your choice. It is designed in such a way that once a certain amount of coins is locked in there you can use those coins to send and receive Lightning payments. 

You can close a payment channel and get the rest of your locked coins back to your Bitcoin wallet at any time, either by agreeing with a peer on a mutual closing transaction or via forced channel closing if your peer becomes uncooperative.

### [](#forced-channel-closing)Forced channel closing

This is an option which is always available to both you and your peer once a new payment channel is open. No matter what happens a channel can always be closed without peer's permission with the rest of channel balance refunded back to your Bitcoin wallet.

That said, a refunding Bitcoin transaction created by forced channel closing is encumbered in a time lock of 144 Bitcoin blocks so closing a channel uncooperatively will take about one day of waiting before your channel coins are sent back to your Bitcoin wallet.

You may also want to watch [this video](https://www.youtube.com/watch?v=H-WJPjAp5u8) which covers all the ways a payment channel could be closed.

### [](#commit-transaction-fee)Commit transaction fee

Sending each Lightning payment creates a valid Bitcoin transaction which is kept off-chain, in fact it never even leaves your device unless a forced channel closing happens. This commit transaction must have an appropriate fee for Bitcoin network to accept it in a timely manner.

As a consequence an actual amount available for spending in a payment channel will always be slightly less than a nominal amount because it always needs to contain enough funds for a current Bitcoin commit transaction fee should it ever be broadcasted.

Next: [Recovering lost Bitcoin balance](http://lightning-wallet.com/recovering-lost-bitcoin-balance.html#recovering-lost-bitcoin-balance)
