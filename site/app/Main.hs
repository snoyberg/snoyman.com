mkYesod "App" [parseRoutes|
/base
/shekel ShekelR GET
/shekel/feed ShekelFeedR GET
|]

getShekelR :: Handler Html
getShekelR = do
  currentRef <- appShekel <$> getYesod
  ecurrent <- readIORef currentRef
  case ecurrent of
    Left e -> defaultLayout $ do
      setTitle "Dollar versus Shekel—not available"
      [whamlet|
        <p>The exchange rate is not currently available.
        <pre>#{e}
      |]
    Right Current {..} -> defaultLayoutExtra (const ("Updated " <> toHtml date)) $ do
      setTitle "Dollar versus Shekel"
      toWidget [lucius|
        #rateinfo {
            border: 1px solid black;
            border-radius: 7px;
            padding: 10px;
            text-align: center;

            .rate {
                display: block;
                font-size: 200%;
            }
        }
      |]
      [whamlet|
        <div .container>
          <div .row .justify-content-center>
            <div .col-4-lg>
              <p #rateinfo>
                On #{date}, $1 (United States Dollar) buys:
                <span class="rate">#{rate} ₪
                (New Israeli Shekel).
                <span class="delta">The dollar became
                <span class=#{direction}>#{delta}

              <p>You can see more currencies versus the shekel and graphs on <a href="http://www.boi.org.il/en/markets/exchangerates/pages/default.aspx">the Bank of Israel website</a>.
              <form style="border:1px solid #ccc;padding:3px;text-align:center;" action="https://blogtrottr.com" method="post" target="_blank">
                  Enter your email address:
                  <br>
                  <input type="email" style="width:400px" name="btr_email">
                  <br>
                  <input type="hidden" value="https://www.snoyman.com/shekel/feed" name="btr_url">
                  <input type="hidden" name="schedule_type" value="0">
                  <input type="submit" value="Subscribe">
              <div>
                  <a href="https://feeds.feedburner.com/DollarVersusShekel" title="Subscribe to my feed" rel="alternate" type="application/rss+xml">
                      <img src="https://www.feedburner.com/fb/images/pub/feed-icon16x16.png" alt="" style="border:0"/>
                  <a href="https://feeds.feedburner.com/DollarVersusShekel" title="Subscribe to my feed" rel="alternate" type="application/rss+xml">
                      Subscribe in a reader
      |]

