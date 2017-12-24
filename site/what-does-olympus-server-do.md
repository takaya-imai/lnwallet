---
layout: default
---

### [](#what-does-olympus-server-do)What does Olympus server do

Lightning Wallet is autonomous but is does not usually work alone since a special server called Olympus helps it with maintenance tasks. 

*Note: wallet nethier depends on nor ever shares any personally identifying information with Olympus and is designed to work perfectly fine even when server goes offline for an indeterminate periods of time.*

Here is what Olympus server does:

- It provides an aggregate fiat prices collected by periodially polling a number of well known exchanges as well as current Bitcoin transaction fee rates which are taken directly from a local full Bitcoin node. *Note: wallet caches this data locally so in case of Olympus going offline fiat prices will likely stay relevant for hours and Bitcoin fees never completely lose their relevance at all.*

- Maintains a list of public Lightning nodes which a wallet uses to open payment channels. *Note: Olympus only provides node search results while payment channel opening and management is done by Lightning Wallet in a completely autonomous manner.*

- Provides partial payment routes by traversing a graph of available payment channels. This relieves wallet of the burden of maintaining such a graph locally which may quickly become computationally infeasible for a phone app but this also introduces privacy implications which need clarifications:

  - Wallet always asks for a *partial* route which starts at your peer's node. This provides a plausible deniability since your peer may have many different wallets connected to it so there is no way for Olympus to tell for sure which exact wallet asks for a route.

  - Wallet knows when a payment is being sent directly to your peer and does not ask Olympus for a payment route in this special case.

  - Recipient may insert an assisted route right into it's payment request and wallet is smart enough to detect and use it. Here is an example: suppose a complete route is `A (you) → B (peer) → C → D → E (payee)`, in case when payee adds an assisted `D →  E` route into it's payment request an Olympus will be asked to provide only `B (peer) → C → D` part so it would know neither who is a sender nor who is a receiver here.

- Stores an encrypted payment channel backups which can be used to [reimburse your locked channel balance](reimbursing-funds-locked-in-a-lost-payment-channel.html#reimbursing-funds-locked-in-a-lost-payment-channel) if you lose an access to your phone. *Note: saving one backup requires one [storage token](storage-tokens.html#storage-tokens).*

- Schedules delayed refund Bitcoin transactions which spend funds from a payment channel to your Bitcoin wallet in a case of a [forced channel closing](using-lightning-wallet.html#forced-channel-closing). *Note: your wallet is capable of spending these transactions all by itself but it's possible that you lose your phone before a refund time lock is cleared so Olympus acts as a safety belt here. This service also requires one [storage token](storage-tokens.html#storage-tokens).*

### [](#setting-up-your-own-server)Setting up your own server

There exists an Olympus server instance which is maintained by wallet developer and used by every wallet by default but it is always possible to set up your own Olympus. Server code and installation manual is available at [Olympus GitHub repository](https://github.com/btcontract/olympus).
