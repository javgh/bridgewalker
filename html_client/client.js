$(document).ready(function () {
    var uri = 'ws://localhost:9160/';
    var ws = new WebSocket(uri);

    ws.onopen = function() {
        ws.send('Hi there!');
    };

    ws.onmessage = function(event) {
        var p = $(document.createElement('p')).text(event.data);
        $('#messages').append(p);
    }
});
