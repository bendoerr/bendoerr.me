---
author: Ben Doerr
tags: bitcoin
category: Other
title: Bitcoin Explained
---

My Mother-in-law heard about Bitcoin the traditional way, by consuming a
sequence of flashing pictures. So she did the traditional thing and asked her
technical Son-in-law for more information. I wrote a little response avoiding
the technical nature of the phenomenon. But I feel that it deserves a bit more
exploration for both my understanding and yours. Mostly for mine. Maybe yours.

<!--MORE-->

P.S. I know this post is long and needs pictures. (Thats pre-script.) I don't
know just deal with it. It wont hurt you if you don't see any sparkly images
for a few minutes.

Introduction
--------------------------------------------------------------------------------

Bitcoin is fascinating (if you google you will see that word often). A few
years back, the technology community was aflutter with articles;blogs;tweets
about Bitcoin and the mining of those coins, allow me the liberty of calling
them coins. Those few that took the initiative to understand and run some
software on dedicated hardware 24/7 for those years are now rewarded with a
mind boggling net worth. Bitcoin was worth approx. zero dollars in recently
history. In recent history Bitcoin didn't even exist, now, right at this very
moment, as I type, as some news outlet somewhere is writing about it, if I had
a bitcoin, someone would pay me $137.68860 US-American-Dollars for it; it being
a 256-bit number. Do you agree that is insane? Awesome? Confusing? possibly,
maybe *Fascinating*?

✔ Choice D - All of the Above, Thank You Very Much!

Regardless of the success or failure of these numbers/bitcoins. My heart does a
little dance when I think about them. My Father-in-law and I and often keen on
reading science fiction about life around and after approaching singularities.
One of the key singularities that takes place is in our economic systems, an
economics 2.0 one might say. An example being Star Trek's economic system or
lack of economics from our view since they have solved the scarcity problem
with replicators. And while Bitcoin is fully, totally, completely rooted in our
modern economics system; scarcity is the reason it has value. It takes some
huge leaps in terms of how it establishes trust and ownership paving the way
for a non economic singularity, one in which a physical material presence is
not needed.

Modern Currency or Modern Commodity?
--------------------------------------------------------------------------------

Before jumping into the technical details, I must, required by my conscious,
take issue with Bitcoin's branding. This notion of currency, it vexes. While
not an expert here, so please correct me. Bitcoin is a currency as Gold is a
currency. A century+ ago. The Gold Standard, we all know how that worked out.
We will see why as this continues on, however, let me note a few key points.
Bitcoin is apolitical, it is decentralized. There is a set, hard limit,
unchangeable amount of bitcoins. Bitcoins increase at a known rate. A
relatively small group controls the vast majority of Bitcoins as well as
*trust*, I mentioned those miners who started years ago. Given these points,
Bitcoin is unavoidable deflationary and the Miner Aristocracy, whom has not yet
but could be unpredictable or compromised, I wouldn't put much worth into
Bitcoin as a long-term currency.

I think the comparison to Gold is very good. Gold is a modern day commodity.
You don't use it for day to day transactions, you hoard it and sell it when
prices are high. The aristocracy I mentioned are people who control a majority
of the mining horsepower, these people control what is referred to as the
longest blockchain, the consensus of who owns what, this is important as we
will see. They can be likened to the 1800's gold barons, but with the power to
decided who can and cannot use the Bitcoins after they have dolled them out.

The internet is filled. In my case with economists, who can use bigger words to
detail the issues with Bitcoin as a currency. These words could require a
college degree in economics, beware of your eyes. I liked
[this](http://www.nakedcapitalism.com/2013/04/yanis-varoufakis-bitcoin-and-the-dangerous-fantasy-of-apolitical-money.html)
article which left my vision intact. It might just be better if you read that
since I just reread the past two paragraphs and they weren't entirely coherent.

Bitcoin
--------------------------------------------------------------------------------

While Bitcoin isn't the first, it is certainly is the most successful thing
called a _crypto-currency_. Crypto-currency is just a fancy way to say that it
is based on cryptography rather than the physical stuff. Bitcoin didn't invent
anything new, it just took a bunch of known, proven pieces and put them
together in a new way. That leads to huge advantage over other digital and
crypto-currencies that use new and unproven algorithms.

If you want to use Bitcoin, you will need some software or pay someone for some
software that _the_ community has deemed a wallet. Wallets can be associated
with addresses. An address is like a bank account, and identifies you. Wallets
are used to manage and transact bitcoins between addresses. That huge US dollar
exchange rate from before comes from a website called [Mt. Gox](mtgox.com)
where you can basically cash in or out.

Now the rest of this post is going to be dedicated to understanding how we get
Bitcoins and how transactions work if we don't have a central authority like a
bank to track balances and mediate transfers. It will take a bit, but I hope
you stick with me.

Actually I lied, and am going to plagiarize [in full](https://plus.google.com/103808128853828210342/posts/Uy5MD8bZcT3) some guy named [John Watkinson](https://plus.google.com/103808128853828210342/posts). This covers everything I was going to talk about anyway, maybe (for sure) even better than how I was going to talk about it.

> ##### Proof of Work
> How can we trust a loosely-confederated system of computers with
> no central authority to transact money safely and securely? There seems to be
> too much opportunity for an attacker to take advantage of the system. It is not
> an easy thing to overcome, but the idea is that if the majority of these
> systems are honest (or at least not all colluding together), then it can be
> done, using a concept called proof of work.

> Consider the following analogy. Most people are familiar with "Where's Waldo?"
> (originally titled "Where's Wally?" in the UK). It is a series of puzzles
> consisting of very detailed drawings. Somewhere in that drawing is the Waldo
> character, and it is the goal of the reader to find Waldo in each puzzle.
> Imagine a really large "Where's Waldo" puzzle, the size of a billboard. It may
> take hours and hours to find Waldo in that puzzle. However, once you find him,
> you can measure his co-ordinates and relay them to somebody else. This person
> can immediately confirm that you did indeed find Waldo. This is the crucial
> requirement for a "proof of work" problem; that it be difficult to solve, but
> very easy to verify.

> Now of course, it is possible to get lucky and very quickly find Waldo in a
> puzzle. Everybody has probably had this experience once in a while. But it is
> not possible to consistently get lucky this way, and any one Waldo-seeker will
> eventually have to put in the time to solve a lot of puzzles.

> Bitcoin runs on puzzles that are essentially very similar to this. The puzzles
> are called blocks. Each block is very hard to solve, but easily verified once
> solved. Furthermore, each new block is linked to the previous block, forming a
> chain. It is as if when you find Waldo in a puzzle, he is holding a tiny little
> sign that tells you where in the world the next puzzle is. Unless somebody
> finds Waldo in this puzzle, nobody can work on the next puzzle. Also, these
> blocks are computer-generated, and so no person knows where Waldo is at the
> onset. They only way to find him is for everybody to start looking.

> If there are a great many computers working to solve these blocks, then the
> chain of blocks will grow pretty quickly, relative to a few computers toiling
> away in obscurity. And if these computers are running the honest and
> unadulterated Bitcoin software, then the chain of blocks they solve will be the
> "right" ones (they will contain valid and honest balances and transactions). If
> an attacker (or coalition of attackers) are trying to include falsified
> ("wrong") blocks in the chain, they will not be able to keep up with the rate
> of block-solving that the majority of honest workers produce. The result is
> that the longest chain of blocks can be relied upon to be correct and honest.
> Only if the attackers could somehow collectively control more computing power
> than the honest workers could they falsify the system. As long as the Bitcoin
> community is thriving (as it is today), this hijacking scenario is seen as
> being nearly impossible.

> ##### Mining
> It remains now to understand who is doing this work on the blocks and
> why. The computers that run the software that solves these blocks are called
> nodes, and they are operated by miners. Miners are incentivized to solve the
> blocks, because every time they "find Waldo" they are awarded a small prize of
> Bitcoins, an award that is encoded right in to the newly-solved block. The
> award amount per block slowly decreases over time, and will eventually drop to
> zero by the year 2140, at which time 21 million Bitcoins will have been
> created. There are just over 11 million Bitcoins in existence today.

> What ties this all together is that the Bitcoin transactions between addresses
> are broadcast to all the nodes, and they get included in the blocks. So, if two
> people agree to transact some coins, this intent is broadcast to the miners'
> nodes. Public Key Cryptography is used to ensure that the user spending the
> Bitcoins controls the indicated address, the same technology that is used for
> other secure web transactions. The miners will include the transaction into the
> next block if it is valid (the sending address has the required amount of
> Bitcoins, addresses and cryptographic signatures are valid, etc.) and then the
> transaction will become fixed in the block chain. For the parties involved in
> the transaction, they should not yet assume the transaction is completed once
> it is included in a solved block, but rather should wait until several more
> blocks have been appended to the chain, as this will ensure that their
> transaction has definitely made its way onto the longest and therefore official
> (consensus) chain. Each additional block that gets added after the one that
> includes the transaction reduces the chance that the transaction block was
> somehow hijacked by an attacker. The convention is to wait for six blocks to be
> added before considering the transaction to be fully validated, with the first
> of those six containing the transaction. This takes approximately an hour.
> However, many Bitcoin merchants will accept a transaction as valid much earlier
> than this (even instantaneously), and absorb the risk that a small fraction of
> them may be fraudulent. This is not unlike the risk merchants take with
> chargebacks on credit cards.

> ##### Transactions
> From the point of view of the user, much of the above is done
> automatically, including doing the cryptographic verification and waiting for
> six blocks to formally confirm a transaction. Use of Bitcoin online doesn't
> feel all that different from paying online with other methods, such as credit
> card or PayPal. However, unlike regular cash, Bitcoin is arguably not
> well-suited to in-person transactions. If two people exchange conventional cash
> in person, they can visually verify that they received legitimate-looking legal
> tender. With Bitcoin, what would be exchanged instead would be the private key
> of a Bitcoin address. The recipient could verify with a mobile device that the
> address had the correct amount of cash at that moment, but there would be no
> way to ensure that the payer wouldn't immediately double-spend the cash, since
> they could still have a copy of the address's private key. To guard against
> this, the recipient would have to immediately (probably with a mobile device)
> transfer the funds out of the address to a new address under the recipient's
> exclusive control. Only after that transaction was verified could payment be
> assured.

> ##### Transaction Fees
> The health and security of the entire Bitcoin ecosystem
> depends on the miners, as they collectively push the consensus block chain
> along, process transactions and mint new coins. But since the Bitcoin awards
> per block will eventually run out, it begs the question of why anybody would
> continue to bother spending computing resources on mining. The answer is that
> users can set aside an optional fee when conducting a transaction. Whoever
> mines (solves) the block containing that transaction can claim this fee.
> Currently, it is possible to conduct most Bitcoin transactions without paying
> any kind of transaction fee, as the mining community is primarily focused on
> the mined Bitcoins and not the fees. However, as the currency becomes more
> popular and the Bitcoin mining rewards decline further, there will need to be
> an incentive for miners to include transactions into a block. This should
> create a market for transaction fees. If a transaction has a very low or zero
> fee, then it may take a very long time to be confirmed into a block, if ever.
> However, a transaction with a large fee will very likely be confirmed quickly.

> ##### A Few Other Details
> Individual Bitcoins have become quite valuable, but they
> can be subdivided into very small denominations of up to eight decimal places.
> There have been some physical "Bitcoins" minted, but they are really just
> clever ways of hiding a private key so that it can only be accessed once, after
> which the "coin" is destroyed.

> Also, there is a common but erroneous belief that Bitcoin is completely
> anonymous. While the addresses that are exchanged over Bitcoin are not
> personally-identifiable, Bitcoin was not designed with anonymity in mind. The
> full log of all Bitcoin transactions is generally available, although older
> records may be harder to find. Could governments track Bitcoin transactions
> back to their owners by network activity, possibly correlating this with other
> data sources? The answer is yes, they probably can.

Conclusion
--------------------------------------------------------------------------------

While I see a few, might I say, glaring issues with the economics of Bitcoin.
The technical thingamajigairy is super cool. I don't think, personal sentiment
here, that Bitcoin will survive as a player in the longterm digital currency
battles, it's progeny will somehow solve many of the basic issues that we
solved out here in the physical world, plus more. Will that mean there will
have to be some sort of centralized authority? And if so how do we establish
trust in that authority without the backing of a government? Are just some of
the questions that will have to be answered. 
