---
layout: default
---

### [](#reimbursing-funds-locked-in-a-lost-payment-channel)Reimbursing funds locked in a lost payment channel

A phone with an open payment channel may be lost or destroyed at any moment. Once that happens there are two tasks to accomplish: 

1. Bitcoin balance should be restored on a new phone which is relatively simple thanks to [mnemonic code](setting-up-bitcoin-wallet.html#mnemonic-code).
2. Funds locked in a [payment channel](using-lightning-wallet.html#payment-channel) should be reimbursed as well which is not really possible *unless* some special measures are taken *before* an emergency.

Lightning Wallet does take those measures: once a new payment channel is created it's static parameters are automatically encrypted and sent to [Olympus server](what-does-olympus-server-do.hmtl#what-does-olympus-server-do). This operation is only carried out once while no personally identifying information is transferred.

Thanks to this after Bitcoin balance is restored you can just issue a lost channel funds recovery request:

`App Menu → Wallet settings → Recover channel funds`

...and wait for a few seconds before remaining balance from a lost payment channel is refunded to your Bitcoin wallet. Please note that following conditions have to be met for this to work:

- Olympus server does not store your data for free so saving static channel parameters requires one [storage token](storage-tokens.html#storage-tokens).
- A peer on the other side of the lost channel has to be online and well behaving in order to receive your recovery request and initiate a remote [forced channel closing](using-lightning-wallet.html#forced-channel-closing).
