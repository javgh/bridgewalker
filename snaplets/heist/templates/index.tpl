<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="utf-8" />
    <meta name="description" content="Bridgewalker - euro-denominated wallet for the Bitcoin economy" />
    <title>Bridgewalker - euro-denominated wallet for the Bitcoin economy</title>
    <link rel="shortcut icon" href="img/favicon.png" type="image/png" />
    <link rel="stylesheet" href="css/screen.css" />
    <link rel="stylesheet" href="css/font-awesome.css" />
    <link rel="stylesheet" href="css/jquery.fancybox.css" />
    <link rel="stylesheet" href="https://fonts.googleapis.com/css?family=Oswald:700">
</head>

<body id="android2">
    <div id="intro">
        <div id="intro-inner">
            <h1 id="logo">Bridgewalker Bitcoin Wallet</h1>

            <p>
            Bridgewalker is a euro-denominated wallet for the Bitcoin economy.
            Send and receive bitcoins, but maintain your balance in euros.
            Bridgewalker converts back and forth between the currencies just
            when you need to send bitcoins or receive them.
            </p>

            <p>
            Bitcoin's strength is fast and cheap world-wide money transfer with
            zero risk of identity theft. Use Bitcoin as a digital payment
            mechanism, without worrying about its exchange rate!
            </p>

            <ul id="features">
                <li>
                    <h2><i class="icon-file"></i> Open Source</h2>
                    Both the Bridgewalker server as well as the Android client
                    are <a href="/blog/2014/01/07/bridgewalker-open-source/">open source</a>.
                </li>
                <li>
                    <h2><i class="icon-money"></i> Fees will only go down</h2>
                    Fees for this service are 0.75 % on each deposit and
                    withdrawal (i.e. 1.5 % for a "round trip"), but will go down
                    in the future as volume increases.
                </li>
                <li>
                    <h2><i class="icon-share-alt"></i> Free off-chain transactions</h2>
                    Transactions between Bridgewalker users are free and
                    instantaneous.
                </li>
                <li>
                    <h2><i class="icon-wrench"></i> Beta software</h2>
                    Please note: This software is beta and only a technology
                    demo. Use at your own risk. No guarantees of any kind at
                    this point.
                </li>
            </ul>

            <div id="phone"></div>

            <a href="img/qr.png" class="qr fancybox"><span>View QR Code</span></a>
        </div><!-- /intro-inner -->
    </div><!-- /intro -->

    <div id="wrapper">
        <div id="content">
            <!-- navigation -->
            <ul id="navigation">
                <li class="tabs-selected"><a href="#tab-1">Tutorial</a></li>
                <li><a href="#tab-2">Screenshots</a></li>
                <li><a href="#tab-3">FAQ</a></li>
                <li><a class="blog-redirect" href="#tab-4">Blog</a></li>
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
                        guest accounts (which offer only rudimentary backup
                        functionality) and beta state of the software, please
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
                        after which they will be exchanged for euros at the current
                        market price and credited to your account.
                        </p>
                    </li>

                    <li>
                        <a href="img/bridgewalker_screenshot4_send_example.png" class="fancybox" rel="tag" title="Sending bitcoins with Bridgewalker">
                            <img src="img/bridgewalker_screenshot4_send_example_thumbnail.png" alt="" class="right" />
                        </a>

                        <h2>Sending bitcoins</h2>

                        <p>
                        Use the tab "Send" to make a Bitcoin payment. Either
                        enter the recipient's Bitcoin address manually, scan it
                        from a QR code or receive it via NFC. When entering the
                        amount, you can choose between specifying it in bitcoins
                        or in euros and Bridgewalker will do the currency
                        conversion for you.
                        </p>

                        <p>
                        When giving a euro amount, the recipient will receive
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
                        efficient in general, these fees should go down.
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
                            current market bids (i.e. "how much EUR would the
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
                        above the "Send payment" button. A percentage difference
                        between them is calculated, to give you an idea of the
                        overhead introduced by the currency conversion (exchange
                        fees and market spread).
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
                <a href="https://bitcointalk.org/index.php?action=profile;u=4070">jav</a>.
                </p>

                <h3>Q: Why Bridgewalker?</h3>
                <p>
                Bitcoin is both a payment mechanism and a currency. The payment
                mechanism works today: It offers fast, cheap, world-wide digital
                payments without the problems of identity theft, as it is
                cash-like and pseudo-anonymous. It is also the first digital
                payment system without middlemen and therefore is a good
                candidate for being adopted as a neutral protocol, free of
                gatekeepers.
                </p>

                <p>
                Bitcoin the currency, on the other hand, is still very much an
                experiment. Being a young currency, it experiences high
                volatility and economists argue about its long-term viability to
                be used as the sole currency of an economy.
                </p>

                <p>
                To continue this experiment, I believe it is necessary to
                develop solutions that are usable today and especially address
                the market volatility. On the merchant side, services like
                <a href="https://bitpay.com/">BitPay</a> and
                <a href="https://coinbase.com/">Coinbase</a> offer protection
                from exchange rate risk. Bridgewalker is the counterpart for
                customers, offering euro-denominated accounts with easy access
                to the Bitcoin network.
                </p>

                <p>
                The combination of these tools allows the use of Bitcoin as a
                payment mechanism, without being affected by exchange rate
                swings. Increased use of Bitcoin will hopefully help to
                stabilize it over time, so that maybe some day these crutches
                will no longer be necessary.
                </p>

                <h3>Q: How secure is Bridgewalker?</h3>
                <p>
                Security is a core priority in the design of the Bridgewalker
                software stack and the server component is written in Haskell,
                which is very helpful in building robust and secure systems. The
                production servers are configured to have a very small attack
                surface and two-factor authentication is utilized in several
                places. All communication between client and server is encrypted
                and the client generates a strong, random password for its
                account.</p>

                <p>I can point to my track record with Instawallet, which had no
                security incident during the time I ran it (from launch in April
                2011 until March 2012), although admittedly Paymium had less
                luck later on. That said, Bridgewalker is currently in beta, so
                proceed with caution.
                </p>

                <h3>Q: Are you taking a gamble on Bitcoin's exchange rate?</h3>
                <p>
                No, I do not take on any exchange rate risk! I maintain Bitcoin
                short positions equivalent to the total amount of bitcoins
                deposited. Therefore everything is fully hedged.
                </p>

                <h3>Q: What are green addresses?</h3>
                <p>
                Green addresses are a convention by which a sender uses one of
                his addresses in every one of his transactions, thereby
                signaling the origin of the transaction to a recipient who
                recognizes the address. If the recipient trusts this specific
                sender to not attempt to double spend, the recipient may treat
                the funds as confirmed the moment they arrive. See the
                <a href="https://en.bitcoin.it/wiki/Green_address">wiki page</a>
                for more details.
                </p>

                <p>
                Bridgewalker has support for green addresses in theory, although
                currently (December 2013) no specific addresses are configured.
                Previously Mt.Gox Bitcoin transactions were recognized in this
                manner, but Mt.Gox discontinued its support for green addresses.
                All outgoing Bridgewalker transactions can be recognized by the
                green address 1MAxx46Dp3tFw933PxPwEYYGCpxYda2pyH.
                </p>

                <h3>Q: Is Bridgewalker compatible with SatoshiDice?</h3>
                <p>
                No, it is not. Because Bridgewalker uses a shared wallet, any
                winnings that are sent back to Bridgewalker can not be
                attributed to your account. You will need to use a different
                wallet, if you intend to gamble on SatoshiDice.
                </p>

                <h3>Q: Will there be a version of Bridgewalker for iOS?</h3>
                <p>
                I would like to support iOS, but it seems a little unclear to me
                at the moment, whether such an app would be allowed in the App
                Store. I am reluctant to invest the effort to create such an
                app, if there is a considerable risk that it will not be
                approved. Things like Cydia do not seem like much of a solution
                to me, as only a small percentage of people actually jailbreak
                their device, from what I can gather.
                </p>

                <h3>Q: Will there be a desktop version of Bridgewalker?</h3>
                <p>
                Most likely at some point, although it is not a priority at the
                moment. The current Android client talks to the server over a
                websocket connection, so either an HTML5 client or some type of
                desktop client would also be possible instead. Bridgewalker is
                open source and the
                <a href="https://github.com/javgh/bridgewalker/blob/master/doc/api.md">server API</a>
                is documented - feel free to have a go at it, a third-party
                desktop client would be very welcome!
                </p>
            </div><!-- /tab-3 -->

            <div id="tab-4">
                Read more about Bridgewalker on its <a href="/blog">blog</a>.
            </div><!-- /tab-4 -->
        </div><!-- /content -->

        <div id="sidebar">
            <a href="https://play.google.com/store/apps/details?id=com.bridgewalkerapp.androidclient" id="buy">Get Bridgewalker!</a>

            <h3>Contact me</h3>

            <ul>
                <li>Jan Vornberger</li>
                <li><a href="mailto:jan@uos.de">jan@uos.de</a></li>
            </ul>

            <h3>Follow me</h3>

            <ul id="social">
                <li><a href="https://www.twitter.com/bridgewalkerapp" id="twitter">Twitter</a></li>
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

    <a href="/blog/2013/12/29/hive-acquisition/"><img style="position: absolute; top: 0; right: 0; border: 0;" src="img/hive_ribbon.png" alt="Acquired by Hive"></a>
</body>
</html>
