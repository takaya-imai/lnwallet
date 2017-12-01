---
layout: default
---

### [](#storage-tokens)Storage tokens

These are special cryptographic entities provided by Olympus server in exchange for a Lightning payment. You can think of them as one-off tickets which can be used to anonymously store such data as [channel backups](reimbursing-funds-locked-in-a-lost-payment-channel.html#reimbursing-funds-locked-in-a-lost-payment-channel) and [scheduled Bitcoin transactions](what-does-olympus-server-do.html#what-does-olympus-server-do) on Olympus server.

Currently an automatic Lightning payment of 2000 Satoshi to buy 50 storage tokens is issued by wallet when a first [payment channel](using-lightning-wallet.html#payment-channel) is created. Once obtained these tokens are stored in a wallet while being spent gradually and automatically as the need comes by.

### [](#reasons-for-storage-tokens-to-exist)Reasons for storage tokens to exist

- There is no incentive for me the developer to maintain a public Olympus server without getting paid for it somehow. Accepting micropayments for some useful API calls seems like a good idea but using a Lightning payment per each call is an overkill.

- The goal is to keep Lightning Wallet as private as possible so any kind of traditional approaches like registration and user accounts is a no-go. But at the same time simply allowing anyone to store any amount of data on server opens a door for multiple kinds of spam attacks.

Storage tokens solve both of these issues beautifully: a single Lightning micropayment gives you an access to 50 paid API calls and technically speaking these storage tokens are [blind signatures](https://en.wikipedia.org/wiki/Blind_signature) which posess an [unlinkability property](https://tools.ietf.org/id/draft-hansen-privacy-terminology-00.html#unlinkability) so every API call made by your wallet is anonymous and private.

All that said, you can opt out of this at any moment by [setting up your own Olympus server](what-does-olympus-server-do.html#setting-up-your-own-server) which would use a key based authentication instead of tokens described here.
