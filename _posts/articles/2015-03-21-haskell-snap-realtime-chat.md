---
layout: post
title: Building a Realtime Web Chat in Haskell with Snap
excerpt: Realtime chat with Haskell + Web sockets!
categories: articles
tags: [haskell,functionalprogramming,snapframework,real,world,fp,lambda]
comments: true
share: true
redirect_from: /post/114214290439/haskell-snap-realtime-chat
---

<p>A few people have let me know recently that it&rsquo;s interesting to see glimpses of Haskell and its power, but they wanted to see more. What I mean is, they wanted to see Haskell used in a <i>real</i> setting. Barring projects such as <a href="http://xmonad.org/" target="_blank">xmonad</a>, I will agree that there seems to be fewer <i>popular</i> open source use cases of Haskell that are &ldquo;full&rdquo; products. This could be because no one thinks Haskell is &ldquo;practical&rdquo; to work on real world problems since it is a <i>pure </i>functional language and the problems we solve as software engineers are naturally <i>impure</i> due to the world we live in. But I won&rsquo;t speculate anymore on that. Instead, I am going to present a sort of tutorial on how to create a real web service (i.e. realtime chat) using the <a href="http://snapframework.com/" target="_blank">Snap Framework</a> with a <a href="http://www.postgresql.org/" target="_blank">PostgreSQL</a> backend.</p><p>You can view the whole source code <a href="https://github.com/DeathByTape/snap-webchat" target="_blank">here</a>.<br/></p><h2>Why Snap and Postgres?</h2><p>The first question we must ask ourselves is simply why we are using a particular tech stack. In this case, it&rsquo;s because Snap is relatively lightweight (compared to other powerful frameworks such as <a href="http://www.yesodweb.com/" target="_blank">Yesod</a>) and very easy to get started with. Beyond that, there are nice &ldquo;snaplets&rdquo; which support Postgres and authentication right out box. Similarly, there is even a type-safe library we will use known as <a href="https://hackage.haskell.org/package/opaleye" target="_blank">Opaleye</a> for building PostgreSQL database queries.</p><p>In short, the stack provides us with a lot of implementation for &ldquo;free&rdquo; which is exactly what we want when we&rsquo;re prototyping a new idea. We don&rsquo;t want to get bogged down into the details of creating a user management system for the 100th time before we can get to the core of our new idea.</p><h2>So what are we building?</h2><p>With the help of some Javascript, we will build a real-time chat system that retains a chat log and can be reviewed later. In building this service we will explore some of the more interesting components of Snap and why they are useful. </p><h2>Brief Stack Overview</h2><p>Very briefly, our technology stack is going to look like this:</p><ul><li><b>Backend</b></li><ul><li><a href="http://snapframework.com" target="_blank">Snap Framework</a> (web server/framework)</li><li><a href="http://postgresql.org" target="_blank">PostgreSQL</a> (database)</li><li><a href="https://hackage.haskell.org/package/opaleye" target="_blank">Opaleye</a> (SQL DSL for Haskell)</li></ul><li><b>Frontend</b></li><ul><li>Plain Javascript/HTML</li></ul></ul><p>You will notice that our frontend stack looks lighter than usual. That is simply because the focus here is on the backend and we will only be providing a very primitive interface. Our backend will be served up in Haskell. This should provide us with the benefits of type-safety and terseness if we do it correctly.</p><h2>Where to Start?</h2><p>Basically we need to setup our development environment before we go any further. <a href="http://google.com" target="_blank">Google</a> is a great resource for this information, so I will provide you with a few pointers and you can follow the links from there. In short, you will want the <a href="https://www.haskell.org/platform/" target="_blank">Haskell Platform</a> (<b>NOTE:</b> If you&rsquo;re on mac, I suggest you use <a href="https://ghcformacosx.github.io/" target="_blank">ghc for OS X</a>). Once you have that installed, you will want to run the following from the command line:</p><pre class="brush: bash">cabal install snap</pre><p>This will install the snap framework binary to help us initialize a project. For those of you who aren&rsquo;t aware, <b>cabal</b> is the build and package manager for Haskell projects (think gnu make and maven).</p><h2>Cabal Sandbox</h2><p>First, create a new working directory by either cloning the <a href="https://github.com/DeathByTape/snap-webchat" target="_blank">existing repo</a> or&ndash; if you want to do it you self&ndash; running <b>snap init</b> (should have been installed by cabal) in an empty directory. Immediately after this, I suggest setting up a sandbox in cabal by running <b>cabal sandbox init</b>. In short, this will give you a fresh environment to run a fresh build on your software.</p><p>There are many great features to cabal and there exist many great resources on the web. Among them, <a href="http://coldwa.st/e/blog/2013-08-20-Cabal-sandbox.html" target="_blank">this guide</a> is useful for getting started with Cabal sandboxes.</p><h2>Chat Design</h2><p>Before we start digging into code, let&rsquo;s first consider the design for our chat server. Below is a diagram to describe what we will be implementing.</p><p style="text-align:center"><img src="/images/rt-chat-backend-arch.png" alt="image"/></p><p>Our architecture is relatively straightforward. As before (while looking at the tech stack), the frontend looks very familiar. In fact, nothing has really changed from the perspective of developing our client-side app.</p><p>On the server-side, we notice that we have divided our system into two distinct logical components (these will correspond to <a href="http://snapframework.com/snaplets" target="_blank">snaplets</a>). For this use case, we will use the provided user authentication snaplet that comes with the Snap framework to be our user management controller. That leaves the chat snaplet for us to develop. In particular, the chat snaplet is responsible for broadcasting real-time chat messages back out to its clients as well as updating the database with the information it has received.</p><p>Moreover, we have to design some logical schema for our <i>database</i>. In summary, we will use a simple schema which has the following columns:</p><ul><li>id (unique identifier for message)<br/></li><li>user_id (foreign key referencing user who posted message)</li><li>message (message content)</li><li>date_posted (datetime of posting)</li></ul><p>You can view the SQL schema <a href="https://github.com/DeathByTape/snap-webchat/blob/master/bootstrap/chat_schema.sql" target="_blank">here</a>.</p><h2>The Server</h2><p>Our basic server implementation has two major components.</p><ol><li>Relay chats to all connected users</li><li>Log chats in database</li></ol><p>From the perspective of the server, we use two async threads and race them (both threads block if nothing occurs). Thread (1) listens for new messages from the user and thread (2) listens for new messages from someone else. If (1) wins the race, then the server adds the message to a pubsub channel and stores the result in the database. If thread (2) wins, then the server sends the message on to the client. We do it this way since&ndash; at the time of writing&ndash; there was no proper way I could find to perform something like a POSIX select() on our channels.</p><p>The implementation of our server sounds pretty straightforward and indeed it is. See below.</p>

```haskell
chatServer :: UserIdentity -> Handler b Chat ()
chatServer (user, uid) = do
  bchan <- gets bcastChan
  cntRef <- gets userCount
  dbSnaplet <- gets _db
  liftIO $ incCount cntRef
  runWebSocketsSnap $ (flip catch) (handleQuit cntRef) . handler bchan dbSnaplet
  where -- Helper to accept client connection request and setup serving loop
        handler :: BroadcastChannel -> Snaplet Postgres -> ServerApp
        handler chan dbSnaplet pconn = do
          conn <- acceptRequest pconn
          dup <- dupChan chan
          forkPingThread conn 15 -- Check that our user is alive every 15 seconds
          wsReader <- async $ readChan dup
          wsWriter <- async $ receiveDataMessage conn
          serve conn dbSnaplet dup wsReader wsWriter
        -- Helper to actually manages comm among users
        serve :: Connection -> Snaplet Postgres -> BroadcastChannel -> Async LBS.ByteString -> Async DataMessage -> IO ()
        serve conn dbSnaplet chan wsReader wsWriter = do
          result <- waitEither wsReader wsWriter
          case result of
           Left msg -> sendDataMessage conn $ Text msg
           Right (Text msg) -> do
             written <- runReaderT (withPG $ liftPG $ storeMessage msg) dbSnaplet
             Prelude.putStrLn $ if written > 0 then "stored message." else "did not store message."
             writeChan chan (LBS.append "<" $ LBS.append user $ LBS.append "> " msg)
           Right _ -> putStrLn "Received some binary data from client. Ignoring."
          -- NOTE: This is ugly.. It continuously creates/tearsdown threads
          -- Determine who won the race and which async we need to restart
          let loop = serve conn dbSnaplet chan
          case result of
           Left _ -> do
             nextReader <- async $ readChan chan
             loop nextReader wsWriter
           Right _ -> do
             nextWriter <- async $ receiveDataMessage conn
             loop wsReader nextWriter
        -- Helper to handle when user quits
        handleQuit :: IORef Int -> ConnectionException -> IO ()
        handleQuit cntRef (CloseRequest _ _) = decCount cntRef
        handleQuit cntRef ConnectionClosed = decCount cntRef
        handleQuit _ e = Prelude.putStrLn $ "Unhandled exception: " ++ show e
        -- Helpers for modifying user count
        updateCount :: IORef Int -> (Int -> (Int, Int)) -> IO ()
        updateCount cntRef fn = atomicModifyIORef cntRef fn >>= Prelude.putStrLn . ("User count: " ++) . show
        decCount cntRef = updateCount cntRef (\x -> let y = x - 1 in (y, y))
        incCount cntRef = updateCount cntRef (\x -> let y = x + 1 in (y, y))
        -- Database helper
        storeMessage msg conn = runInsert conn chatMessageTable $
            ChatMessage' { msgId = Nothing
                         , msgText = (pgString . C.unpack) msg
                         , msgUserId = pgInt4 uid
                         , msgDate = Nothing
</pre><p>We also implement an additional feature (a short 50 message history) to exercise some of Opaleyes features. We demonstrate how easy it is to use Opaleyes Query Arrow to construct a cross-table join to get proper usernames and to store the results in a type-safe manner. The code looks like this:</p>
<pre class="brush: haskell">
-- | Handler to retrieve the last 50 chat messages
getLastFifty :: Handler b Postgres ()
getLastFifty = do
  msgQuery <- return $ ((limit 50 . orderBy (asc msgDate) . queryTable) chatMessageTable)
  userQuery <- return $ queryTable userTable
  msgs <- liftPG $ getMessages $ joinMessagesAndUsers msgQuery userQuery
  writeText $ if Prelude.length msgs > 0
              then T.append (T.append "<pre>" ((T.pack . foldl ((++) . (++"\n"))  "" . fmap formatChatMessage) msgs)) "</pre>"
              else "No messages to display"
  where getMessages :: Query ChatMessageColumn -> PGS.Connection -> IO [ChatMessage]
        getMessages = flip runQuery
        joinMessagesAndUsers :: Query ChatColumnR -> Query UserColumn -> Query ChatMessageColumn
        joinMessagesAndUsers msgQuery usrQuery = proc () -> do
          (ChatMessage' mid txt uid date) <- msgQuery -< ()
          (userid, login) <- usrQuery -< ()
          restrict -< uid .== userid
          returnA -< ChatMessage' mid txt login date
        formatChatMessage :: ChatMessage -> String
        formatChatMessage (ChatMessage' _ txt uid date) =
          "[" ++ show date ++ "] <" ++ uid ++ "> " ++ txt
```

<p></p><blockquote>You will notice that some of the definitions are left out. To see the whole Chat.hs source code, <a href="https://github.com/DeathByTape/snap-webchat/blob/master/src/Snap/Snaplet/Chat.hs" target="_blank">view it on Github</a>.</blockquote>
<p>With the help of some <a href="https://www.fpcomplete.com/school/to-infinity-and-beyond/pick-of-the-week/a-little-lens-starter-tutorial" target="_blank">lenses</a> and <a href="https://www.haskell.org/arrows/" target="_blank">arrows</a>, you will notice how easy this library is to use. I can express my query entirely in Haskell code and also retrieve my result as a list of populated objects. In particular, running the join and projecting only the fields we needed was particularly painless. Writing many &ldquo;SELECT x AS x1, y AS y2 &hellip;&rdquo; statements is not a particularly interesting part of our jobs.</p>

<h2>Wrapup</h2>
<p>Now that we have explored the design and key components of our implementation, you can hopefully see that Haskell <i>is</i> a practical language. Obviously, it has significant power beyond building interactive websites, however, it is also useful in such a situation. This is important seeing as the web probably isn&rsquo;t going anywhere anytime soon. The next time you think about creating a project and you have some spare time to learn along the way, you should pick up Haskell. Though the learning curve may be steeper than in many other languages, the more you use it the easier it becomes. Finally, it <i>will</i> work for your use-case despite how many people claim it&rsquo;s simply not a practical language. Good luck!</p>
