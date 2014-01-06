This is the server component of Bridgewalker ( https://www.bridgewalkerapp.com/ ).

To give a very rough overview: The server is a Haskell application to which
clients ( for example the Android client - see
https://github.com/javgh/Bridgewalker-Android ) can connect to via a websocket
connection. See doc/api.md for a description of the server API. The server
maintains user accounts and other state in a PostgreSQL database and uses a
locally running Bitcoin daemon and an exchange account at Mt.Gox to offer
functionality to the clients. All user deposits are kept in euros, but the
server requires a small bitcoin "buffer" both locally as well as on the exchange
side to be able to quickly send out Bitcoin transactions as well as push
exchange orders through. It will periodically rebalance these buffers should an
imbalance arise.

To get an idea of how everything is supposed to play together, it is probably
best to have a look at the Ansible script which deploys the production server.
See: https://github.com/javgh/bridgewalker-deployment . That repository also
contains a template for the Bridgewalker configuration file (expected at
~/.bridgewalker/config), which looks something like this:

    mtgox_auth_key =
    mtgox_auth_secret =
    key_set = 2
    rpcurl = http://127.0.0.1:8332
    rpcuser = rpcuser
    rpcpassword = localaccessonly
    safety_margin_btc = 0.50
    safety_margin_usd = 5.00
    typical_tx_fee = 0.0001
    maximum_order_btc = 4.00
    mtgox_minimum_order_btc = 0.01
    target_exchange_fee = 0.4
    bitcoind_notify_file = /home/jan/.bitcoin/notify.pid
    marker_addresses = [ ("1LNWw6yCxkUmkhArb2Nf2MPw6vG7u5WG7q", 10)
                       , ("14Z1mazY4HfysZyMaKudFr63EwHqQT2njz", 2)
                       , ("1MAbwuYp8CPChJ1ua25tnEKXkfXTVqEoyg", 10)
                       ]
    stdout_logging = false

The first three lines configure API access to the Mt.Gox account. This is not
quite as straightforward, as it might appear at first, as the keys listed here
will first be combined with hardcoded keys inside the server binary to arrive at
the final keys. This was done to add some extra "security by obscurity". While
security by obscurity is not a valid security concept on its own, it certainly
does not hurt to have it in addition. The thinking was, that should an attacker
gain access to the server's configuration file, that he would still need to
figure out this scheme and then extract the harcoded part of the key from the
binary. This would hopefully slow down or maybe even prevent some less skilled
attackers from escalating access to the exchange account. Now that the source
code is open source, some of that "security by obscurity" is negated of course,
but the mechanism it still active. To bring Mt.Gox API credentials in the
necessary format, use the function `scrambleText` in
src/ScrambleCredentials.hs . For example:

    ghci ScrambleCredentials.hs
    > scrambleText "asdf1234"
    ("asdf0000",[42,84,126,168])

Then put the first part (`asdf0000`) inside the configuration file and set the
second part (`[42,84,126,168]`) as one of the hardcoded keys in
ScrambleCredentials.hs . The constants `hardcodedKeyA` and `hardcodedKeyB` are
used to unscramble `mtgox_auth_key` and `mtgox_auth_secret` respectively if
`key_set` is set to 1. If it is set to 2, the constants `hardcodedKeyC` and
`hardcodedKeyD` are used. This can be used to have two differents sets of
credentials for the staging and production environment.

Lines 3 to 6 specify on how to access the local Bitcoin daemon.

The parameters `safety_margin_btc` and `safety_margin_usd` specify the minimum
balance that should always be maintained for the local bitcoin balance as well
as the exchange bitcoin and fiat balance. This is to ensure that an action does
not fail with "insufficient funds" just because an estimation was off by a few
cents. Please note, that Bridgewalker is currently hardcoded to work on the
BTC/EUR pair for Mt.Gox only. This was done in a very hackish way, which is why
many of the function names still contain USD, because BTC/USD was the previously
used pair.  I apologize for this terrible source of confusion. I had planned to
clean this up, but you know how priorities change around sometimes. So in this
case the parameter `safety_margin_usd` actually specifies an euro amount.

The parameter `typical_tx_fee` specifies the transaction fee (in BTC) that
should be assumed for all outgoing Bitcoin transactions and deducted accordingly
from user balances. The parameter `maximum_order_btc` sets the maximum size of
an outgoing Bitcoin transaction and `mtgox_minimum_order_btc` specifies the
smallest possible order that can be executed on Mt.Gox (currently 0.01 BTC). The
parameter `target_exchange_fee` is used to decide what the minimum fee per
transaction should be in percent. The actual fee the user will have to pay is
always along the lines of `max(exchange_fee, target_exchange_fee)`, so only if
the fee paid to the exchange is lower than this target fee will Bridgewalker
create any revenue.

The parameter `bitcoind_notify_file` is passed on to the Haskell Bitcoin RPC
library ( see here for documentation:
http://hackage.haskell.org/package/bitcoin-rpc ).

The parameter `marker_addresses` lists a number of marker addresses (also called
green addresses) that are trusted to not double spend. Transactions from these
addresses are treated as confirmed instantaneously. Each address is accompanied
by a limit, which specifies the total amount of BTC that can be in "pending"
state (unconfirmed transactions) at any time from this address. If transactions
from this address exceed this limit, they are treated as standard transactions.
You can read more about green addresses here:
https://en.bitcoin.it/wiki/Green_address .

The parameter `stdout_logging` activates logging to STDOUT. Regardless of this
setting, additional logs will always be written to the subdirectory log/.

Finally an additional configuration file called workingfund (also in
~/.bridgewalker) should list a single number, which is the size of the Bitcoin
buffer (in BTC) which will be maintained both locally as well as on the exchange
side. This number needs to be at least as large as `maximum_order_btc` so that
the server is able to send out the largest possible transaction using only the
buffer.
