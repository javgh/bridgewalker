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
    , "account_password": "w52CcxwA3fnmFxpBSaqFy2h39"
    , "account_name": "guest_FQtWSR3S"
    }

The open connection can then be used right away to login using this new account:

    { "op": "login"
    , "account_password": "w52CcxwA3fnmFxpBSaqFy2h39"
    , "account_name": "guest_FQtWSR3S"
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
the account. Refer to the API reference for a description of all fields.
Furthermore, the server will send this status description whenever there are any
changes to the account. The client therefore has to be prepared to receive this
reply unsolicited.

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

## API reference ##

TODO: Note about 'USD'
