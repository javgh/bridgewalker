BRIDGEWALKER API DOCUMENTATION
==============================

This document describes the Bridgewalker Websocket API.

## Getting started ##

The API is available by opening an encrypted websocket connection to the
endpoint `wss://www.bridgewalkerapp.com/backend`. Over this connection a custom
JSON-based protocol is spoken. Each line (terminated by \n) is interpreted as a
JSON-encoded data structure, which describes the command to be executed. Replies
are also JSON-encoded and terminated by a newline character.

The following example shows how to request the server version:

    {"op": "request_version", "client_version": "0.1"}

The resulting reply should look like this:

    {"reply":"server_version", "server_version":"0.1"}

Tip: You can use the echo test application available at
[http://www.websocket.org/echo.html](http://www.websocket.org/echo.html) to
connect to the endpoint and try out the above command.

## Typical session ##

This section describes the commands issued in a typical session, where a client
creates a new guest account, then logins in with it, receives a payment and
finally issues a payment.

After connecting, it is recommended - although not required - to check the
server version:

    {"op": "request_version", "client_version": "0.1"}

    {"reply":"server_version", "server_version":"0.1"}

In this unauthenticated state of the connection, the client can only issue two
commands: Account creation and login. To create a new guest account (full
accounts are not yet implemented), the following command can be used:

    {"op": "create_guest_account"}

    { "reply": "guest_account_created"
    , "account_name": "guest_FQtWSR3S"
    , "account_password": "w52CcxwA3fnmFxpBSaqFy2h39"
    }

The open connection can then be used right away to login using this new account:

    { "op": "login"
    , "account_name": "guest_FQtWSR3S"
    , "account_password": "w52CcxwA3fnmFxpBSaqFy2h39"
    }

    {"reply": "login_successful"}

The next step is to request the current status of the account:

    {"op": "request_status"}

    { "reply": "status"
    , "exchange_rate": 10125165
    , "exchange_available": true
    , "btc_in": 0
    , "primary_btc_address": "1QC14aUkvsoUCHBpNrrpbtNfzoXZA3iEr7"
    , "pending_txs": []
    , "usd_balance": 0
    }

As seen above, this will return a number of things about the current status of
the account. Refer to the API reference for a description of all fields. Amounts
are always represented as integers. Fiat amounts need to be divided by 10 ^ 5 to
arrive at the correct amount (in the example above, the exchange rate is
101.25165 EUR/BTC). Bitcoin amounts are in Satoshis. Note that the API currently
still uses USD in many field names. The reason for this is, that the switch to
Euro has only partially been implemented yet. For the moment all USD fields
simply need to be interpreted as referring to EUR amounts.

The server will send this status description whenever there are any changes to
the account. The client therefore has to be prepared to receive this reply
unsolicited.

Note that in general the client can not rely on the fact, that the first reply
after sending a command is the associated answer. It needs to be prepared for
receiving things out-of-order, e.g. a status update being mixed in before a
specific request is answered.

The server will timeout any clients, that do not send a ping command for more
than 90 seconds. It is recommended to send a ping every 85 seconds:

    {"op": "ping"}

    {"exchange_rate": 10125165, "reply": "pong"}

The pong reply also contains a current exchange rate, to keep the client
updated.

Sending bitcoins to the address associated with the account, should result in a
status update, which might look something like this:

    { "reply":"status"
    , "exchange_rate": 10200000
    , "exchange_available": true
    , "btc_in": 0
    , "primary_btc_address": "1QC14aUkvsoUCHBpNrrpbtNfzoXZA3iEr7"
    , "pending_txs":
        [ { "amount": 1000000
          , "reason": {"type": "too_few_confirmations", "confirmations": 0}
          }
        ]
    , "usd_balance": 0
    }

Before sending out a payment, a quote can be requested to estimate fees and
currency conversion:

    { "op": "request_quote"
    , "request_id": 42
    , "type": "amount_based_on_btc"
    , "amount": 1000000
    }

    { "reply": "quote"
    , "request_id": 42
    , "btc": 1000000
    , "usd_recipient": 101110
    , "usd_account": 102612
    , "sufficient_balance": false
    }

The reply will use the same request id as the request, so that the client can
associate the two. See the API reference for a description of the other fields.

Finally, to send out a payment, the send payment command can be used, which
takes the same argument as the quote request plus a Bitcoin address:

    { "op": "send_payment"
    , "request_id": 42
    , "type": "amount_based_on_btc"
    , "amount": 500000
    , "address": "1AdjqgSMfhUxTmJJdDBL8qFVfAcy3pRJpu"
    }

    {"request_id": 42, "reply": "send_successful"}

## API reference ##

All amounts used in the API are transmitted as integers. Bitcoin amounts are
given in Satoshis. Euro amounts needed to be divided by 10 ^ 5 to arrive at the
correct amount (i.e. 1 EUR is represented as 100000).

Note that the API currently still uses USD in many field names. The reason for
this is, that the switch to Euro has only partially been implemented yet. For
the moment all USD fields simply need to be interpreted as referring to EUR
amounts.

If not specified otherwise, commands are only available after a successful
login.

### Request version ####

Request the server's version. Breaking changes to the API are signaled by a new
major version number (the one before the dot). This command can be executed
while being unauthenticated.

Request:

    {"op": "request_version", "client_version": "0.1"}

Reply:

    {"reply":"server_version", "server_version":"0.1"}

### Create guest account ####

Create a new guest account. The server will choose a random user name and
password for the new account. This command can be executed
while being unauthenticated.

Request:

    {"op": "create_guest_account"}

Reply:

    { "reply": "guest_account_created"
    , "account_name": "guest_FQtWSR3S"
    , "account_password": "w52CcxwA3fnmFxpBSaqFy2h39"
    }

### Login ####

Login using account credentials. This command can be executed while being
unauthenticated.

Request:

    { "op": "login"
    , "account_name": "guest_FQtWSR3S"
    , "account_password": "w52CcxwA3fnmFxpBSaqFy2h39"
    }

Reply in case of success:

    {"reply": "login_successful"}

Reply in case of failure:

    {"reply": "login_failed", "reason": "Unkown account or wrong password"}

### Request status ####

Request the current status of the account.

Request:

    {"op": "request_status"}

Reply:

    { "reply": "status"
    , "exchange_rate": 10125165
    , "exchange_available": true
    , "btc_in": 0
    , "primary_btc_address": "1QC14aUkvsoUCHBpNrrpbtNfzoXZA3iEr7"
    , "pending_txs": []
    , "usd_balance": 0
    }

Description of fields:

 * `exchange_rate`: Exchange rate for EUR/BTC
 * `exchange_available`: Whether currently a stable connection to the partnering
   exchange (for now Mt.Gox) exists. If this is not the case, then outgoing
   transactions will fail.
 * `btc_in`: Amount of fully confirmed bitcoins, that are not yet exchanged for
   Euros. The Android client lists this amount as "x BTC waiting to be
   exchanged."
 * `primary_btc_address`: Main Bitcoin address of the account
 * `usd_balance`: Euro balance
 * `pending_txs`: A list of pending Bitcoin transactions - see below

Bitcoin transactions that are not fully confirmed yet, appear in the list of
pending transactions. This is a JSON array with an entry for each transaction.
There are two types of entries. In most cases, the reason for a pending
transaction will be, that it has not yet enough confirmations. The corresponding
entry looks like this:

    { "amount": 1000000
    , "reason": {"type": "too_few_confirmations", "confirmations": 0}
    }

Bridgewalker supports green addresses and normally transactions coming from a
green address will confirm instantly. However, for security reasons there is a
build-in limit for the amount of unconfirmed bitcoins from a specific green
address. If this limit is reached, the transaction will also be pending until it
has enough confirmations or falls below the limit again (this can happen, if a
previous green transaction confirms in the meantime). The corresponding entry
looks like this:

    { "amount": 1000000
    , "reason": { "type": "marker_address_limit_reached"
                , "marker_address": "1MAxx46Dp3tFw933PxPwEYYGCpxYda2pyH"}
    }

The field `marker_address` lists the green address, whose limit has been
reached.
