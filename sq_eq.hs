main :: IO ()
main =
  if time >= 12 then
    putStrLn "Good afternoon"
  else
    (if _ then _ else putStrLn "Good morning")

time = 13
