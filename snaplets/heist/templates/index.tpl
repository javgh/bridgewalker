<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="utf-8" />
    <meta name="description" content="Bitcoin wallet with a twist: Send and receive bitcoins, but hold US dollar." />
    <title>Bridgewalker Bitcoin Wallet</title>
    <link rel="shortcut icon" href="img/favicon.png" type="image/png" />
    <link rel="stylesheet" href="css/screen.css" />
    <link rel="stylesheet" href="css/font-awesome.css" />
    <link rel="stylesheet" href="css/jquery.fancybox.css" />
    <link rel="stylesheet" href="http://fonts.googleapis.com/css?family=Oswald:700">
</head>

<body id="android2">
    <div id="intro">
        <div id="intro-inner">
            <h1 id="logo">Bridgewalker Bitcoin Wallet</h1>

            <p>
            Bitcoin wallet with a twist: Send and receive bitcoins, but hold US
            dollar. Bridgewalker converts back and forth between bitcoins and US
            dollar just when you need to send some or receive them. Minimize
            your exposure to the young currency's exchange rate risk, while
            taking advantage of its strength for fast and cheap world-wide money
            transfer with zero risk of identity theft.
            </p>

            <ul id="features">
                <li>
                    <h2><i class="icon-user"></i> One-click registration</h2>
                    Use guest mode to get up and running in seconds. Full
                    accounts coming soon.
                <li>
                    <h2><i class="icon-money"></i> Fees will only go down</h2>
                    Fees for the exchange service are currently about 1.5 % for
                    a "round trip" (receive &amp; send), but will go down in the
                    future as volume increases.
                <li>
                    <h2><i class="icon-wrench"></i> Beta software</h2>
                    Please note: This software is beta and only a technology
                    demo. Use at your own risk. No guarantees of any kind at
                    this point.
                </li>
            </ul>

            <div id="phone"></div>

            <!--
            <a href="img/qr.png" class="qr fancybox"><span>View QR Code</span></a>
            -->
        </div><!-- /intro-inner -->
    </div><!-- /intro -->

    <div id="wrapper">
        <div id="content">
            <!-- navigation -->
            <ul id="navigation">
                <li class="tabs-selected"><a href="#tab-1">Tutorial</a></li>
                <li><a href="#tab-2">Screenshots</a></li>
                <li><a href="#tab-3">FAQ</a></li>
            </ul>

            <div id="tab-1">
                <ul class="tour">
                    <li>
                        <a href="img/bridgewalker_screenshot1_login.png" class="fancybox" rel="tag" title="Bridgewalker login screen">
                            <img src="img/bridgewalker_screenshot1_login_thumbnail.png" alt="" class="right" />
                        </a>

                        <h2>Getting started</h2>

                        <p>
                        Use guest mode to create a new account with a single
                        click. A random user name and associated secure password
                        will be generated for you in the background and stored
                        on your device.
                        </p>

                        <p>
                        Full accounts are coming soon, with more functionality,
                        such as setting an extra PIN for additional security and
                        being able to create backups. Given the current limits of
                        guest accounts and beta state of the software, please
                        keep only small amounts in your account.
                        </p>
                    </li>

                    <li>
                        <a href="img/bridgewalker_screenshot2_receive.png" class="fancybox" rel="tag" title="Receiving bitcoins with Bridgewalker">
                            <img src="img/bridgewalker_screenshot2_receive_thumbnail.png" alt="" />
                        </a>

                        <h2>Receiving bitcoins</h2>

                        <p>
                        Switch to the tab "Receive" to see the Bitcoin address
                        of your account, both in text form as well as a
                        scannable QR code. Transactions to this address will
                        require a number of confirmations - typically three -
                        after which they will be exchanged for US dollar at
                        current market price and credited to your account.
                        </p>

                        <p>
                        Bridgewalker also has support for
                        <a href="https://en.bitcoin.it/wiki/Green_address">green addresses</a>.
                        To speed up the deposit process, send from one of the
                        recognized green addresses. Currently this is only the
                        case for Mt.Gox (use their option "Use a Green
                        Address").
                        </p>
                    </li>

                    <li>
                        <a href="img/bridgewalker_screenshot4_send_example.png" class="fancybox" rel="tag" title="Sending bitcoins with Bridgewalker">
                            <img src="img/bridgewalker_screenshot4_send_example_thumbnail.png" alt="" class="right" />
                        </a>

                        <h2>Sending bitcoins</h2>

                        <p>
                        Use the tab "Send" to make a Bitcoin payment. Either
                        enter the recipient's Bitcoin address or scan it from a
                        QR code. When entering the amount, you can choose
                        between specifying it in bitcoins or in US dollar and
                        Bridgewalker will do the currency conversion for you.
                        </p>

                        <p>
                        When giving a USD amount, the recipient will receive
                        bitcoins roughly equivalent to this amount. Because of
                        fees, slightly more will be deducted from your account.
                        If instead you would like to have no more than the
                        specified amount deducted from your account, change this
                        behavior by unchecking the option "Deduct fees
                        separately".
                        </p>
                    </li>

                    <li>
                        <a href="img/info_text.png" class="fancybox" rel="tag" title="Fee estimation">
                            <img src="img/info_text_thumbnail.png" alt="" />
                        </a>

                        <h2>Exchange fees</h2>

                        <p>
                        Bridgewalker currently has a "round trip" fee of about
                        1.5 %, depending on current market conditions. That
                        means, if a friend sends you 1 BTC and you send it back
                        to her right away, she is left with 0.985 BTC. This does
                        not only include the exchange fees Bridgewalker has to
                        pay to its partnering Bitcoin exchange, but is also
                        influenced by the current market spread (difference
                        between highest bid and lowest ask). As Bridgewalker's
                        volume increases and Bitcoin markets become more
                        efficient in general, these fees should go down in the
                        future.
                        </p>
                    </li>

                    <li>
                        <h2>Fee estimation</h2>

                        <p>
                        Bridgewalker always tries to give you an up-to-date
                        approximation of the cost of your outgoing Bitcoin
                        transactions. To make a reasonable estimation it
                        compares two things:
                        </p>

                        <ul>
                            <li>
                            The value of a given BTC transaction in terms of
                            current market bids (i.e. "how much USD would the
                            recipient earn from selling this amount of BTC given
                            the current order book"; possible exchange fees paid
                            by the recipient not taken into account).
                            </li>
                            <li>
                            The cost to you of acquiring the given BTC amount by
                            filling current market asks and paying exchange fees
                            in the process.
                            </li>
                        </ul>

                        <p>
                        These two values are displayed as the recipient's side
                        and your account side, respectively, in the info text
                        above the "Send payment" button. A percentage
                        difference between them is calculated, to give you an
                        idea of the overhead introduced by the currency
                        conversion (exchange fees and market spread).
                        </p>
                    </li>
                </ul>
            </div><!-- /tab-1 -->

            <div id="tab-2">
                <ul class="screenshots">
                    <li><a href="img/bridgewalker_screenshot1_login.png" class="fancybox" rel="tag" title="Bridgewalker login screen"><img src="img/bridgewalker_screenshot1_login_lthumbnail.png" alt="" /></a></li>
                    <li><a href="img/bridgewalker_screenshot2_receive.png" class="fancybox" rel="tag" title="Receiving bitcoins with Bridgewalker"><img src="img/bridgewalker_screenshot2_receive_lthumbnail.png" alt="" /></a></li>
                    <li><a href="img/bridgewalker_screenshot3_send_empty.png" class="fancybox" rel="tag" title="Sending bitcoins with Bridgewalker"><img src="img/bridgewalker_screenshot3_send_empty_lthumbnail.png" alt="" /></a></li>
                    <li><a href="img/bridgewalker_screenshot4_send_example.png" class="fancybox" rel="tag" title="Sending bitcoins with Bridgewalker"><img src="img/bridgewalker_screenshot4_send_example_lthumbnail.png" alt="" /></a></li>
                </ul>
            </div><!-- /tab-2 -->

            <div id="tab-3">
                <h2>FAQ</h2>

                <h3>Q: Who is behind Bridgewalker?</h3>
                <p>
                My name is Jan Vornberger. I run
                <a href="http://www.bitcoinmonitor.com/">Bitcoin Monitor</a> and
                was the original creator of
                <a href="https://www.instawallet.org/">Instawallet</a> before
                giving it over to
                <a href="http://paymium.com/">Paymium</a>.
                I post on the Bitcoin Forum as user
                <a href="https://bitcointalk.org/index.php?action=profile">jav</a>.
                </p>

                <h3>Q: How secure is Bridgewalker?</h3>
                <p>
                That's a tricky question to answer. I like to think that I know
                a thing or two about securing servers, but unfortunately there
                is no such thing as a perfectly secure system. Only the test of
                time will show, whether I managed to secure it well enough. I
                can however point to my track record with Instawallet, which has
                never had a security incident. Security is always a priority in
                designing my software and the server component is written in
                Haskell, which is very helpful in building robust systems. All
                communication between client and server is encrypted and the
                client generates a strong, random password for its account. That
                said, Bridgewalker is currently in beta, so proceed with
                caution.
                </p>

                <h3>Q: Why is the minimum amount for a transaction 0.01 BTC?</h3>
                <p>
                Bridgewalker uses <a href="https://mtgox.com/">Mt.Gox</a> as its
                Bitcoin exchange which currently has 0.01 BTC as the smallest
                amount that can be traded. Bridgewalker might support smaller
                transactions in the future, but for the moment this limit is
                being inherited.
                </p>

                <h3>Q: Will there be a version of Bridgewalker for iOS?</h3>
                <p>
                I would like to support iOS, but it seems a little unclear to me
                at the moment, whether such an app would be allowed in the App
                Store. I am reluctant to invest the effort to create such an
                app, if there is a considerable risk that it will not be
                approved.
                </p>
            </div><!-- /tab-2 -->

        </div><!-- /content -->

        <div id="sidebar">
            <a href="http://themeforest.net/item/app-showcase-iphone-and-mobile-app/179129?ref=myTheme" id="buy">Buy this item!</a>

            <h3>Contact me</h3>

            <ul>
                <li>Jan Vornberger</li>
                <li><a href="jan@uos.de">jan@uos.de</a></li>
            </ul>

            <p id="copyright">Copyright &copy; 2013 Jan Vornberger</p>
        </div><!-- /sidebar -->
    </div><!-- /wrapper -->    

    <!-- JavaScript -->
    <script src="js/jquery_1.8.3.min.js"></script>
    <script src="js/jquery.tabs.pack.js"></script>
    <script src="js/jquery.tipsy.js"></script>
    <script src="js/jquery.validate.pack.js"></script>
    <script src="js/jquery.fancybox.pack.js"></script>
    <script src="js/onload.js"></script>

    <!-- Google Analytics -->
    <script type="text/javascript">

      var _gaq = _gaq || [];
      _gaq.push(['_setAccount', 'UA-39518733-1']);
      _gaq.push(['_trackPageview']);

      (function() {
        var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true;
        ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';
        var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(ga, s);
      })();

    </script>
</body>
</html>
