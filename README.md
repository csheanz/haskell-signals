# haskell-signals

A low-level signal/slots Haskell implementation.

Designed to be similar to Boost.Signals. A simple example:

```
main = do
  
  -- create a new signal
  sig <- newSignal
  
  -- create a new connection.
  connect sig $ \name -> putStrLn ("Hello, " ++ name)
  
  -- create a new connection which will be disconnected soon.
  conn <- connect sig $ \name -> putStrLn ("You won't see this,  " ++ name)
  
  -- disconnect the connection we just created.
  disconnect conn
  
  -- will print "Hello, Tim"
  emit_ ($ "Tim") sig
```
