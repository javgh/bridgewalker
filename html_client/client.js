function display_message(msg) {
    var p = $(document.createElement('p')).text(msg);
    $('#messages').append(p);
}

function request_status(ws) {
    cmd = {"op": "status", "session_id": "onesession"};
    ws.send(JSON.stringify(cmd));
}

function send_btc(ws, address, amount) {
    cmd = {"op": "send_btc", "session_id": "onesession",
                "bitcoin_address": address, "amount": amount};
    ws.send(JSON.stringify(cmd));
}

$(document).ready(function () {
    var uri = 'ws://localhost:9160/';
    var ws = new WebSocket(uri);

    $('#send-form').submit(function() {
        var address = $('#address').val();
        send_btc(ws, address, 1000000);
        $('#address').val('');
    });

    ws.onopen = function() {
        window.setInterval(function() { request_status(ws); }, 2000);
    };

    ws.onmessage = function(event) {
        //display_message(event.data);
        msg = $.parseJSON(event.data);

        if (msg.reply == "status_reply") {
            usd_balance = msg.status.usd_balance / 100000;
            $('#balance').text(usd_balance + " USD");
        }
    }

    ws.onerror = function(event) {
        display_message("An error occured.");
    }

    ws.onclose = function(event) {
        display_message("Connection closed.");
    }
});
