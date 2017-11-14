---
layout: default
---

### [](#recovering-lost-bitcoin-balance)Recovering lost Bitcoin balance

In a nutshell: you need to have a [mnemonic code](setting-up-bitcoin-wallet.html#mnemonic-code) saved *before* your balance is lost, either as plain 12 words or in encrypted form.

A balance typically gets lost when a wallet password is forgotten or when a phone is no longer accessible for whatever reason. Here are the steps to take:

1. If you still have an access to your phone then first of all you should erase all current app data. If you don't know how to erase an app data then just reinstall an application from scratch. If you no longer have an access to your phone then install an app once again on a new phone.

2. Open a fresh application and you'll be given an option to restore an existing Bitcoin wallet instead of creating a new one. Choose it and enter your mnemonic code as well as new wallet password. This is it, no more action is required.

*Important note: restored wallet will appear empty at first and that is completely normal as it needs some time to catch up with network.*

### When wallet is restored but the balance is still zero

If you don't see a balance restored for a long time then first of all you should make sure an app has no connectivity problems. Normally a well connected app displays syncing progress and shows a *Bitcoin wallet is online* message once syncing is done.

A case when an app is indeed well connected and synced but you still see a zero balance and no transaction history means that your mnemonic contains a typo somewhere.

In order to eliminate a typo you can compare each word in your saved mnemonic code with a [list of all possible words](english-bip32.json). You may use an in-page search to check if every word from your mnemonic is present on a page, in not then fix a typo.

And of course you should enter mnemonic code very carefully when restoring a wallet since an app will accept any string while a single typo is a single word will result in totally different wallet.

Next: [Reimbursing funds locked in a lost payment channel](http://lightning-wallet.com/reimbursing-funds-locked-in-a-lost-payment-channel.html#reimbursing-funds-locked-in-a-lost-payment-channel)
