import Database.FeedDB

-- TODO Refacor this
feedDBEntrypont :: IO ()
feedDBEntrypont = do
  -- Initialize the database
  conn <- initializeDB "feeds.db"

  -- Create some sample feed entries
  let sampleEntries =
        [ FeedEntry
            { title = "Haskell is great",
              date = Just "2023-05-15",
              content = "An article about Haskell",
              links = ["http://example.com/haskell", "http://example.com/fp"]
            },
          FeedEntry
            { title = "Learning SQLite",
              date = Just "2023-05-16",
              content = "How to use SQLite with Haskell",
              links = ["http://example.com/sqlite"]
            },
          FeedEntry
            { title = "Functional Programming",
              date = Nothing,
              content = "Introduction to FP concepts",
              links = []
            }
        ]

  -- Insert sample entries
  mapM_ (insertFeedEntry conn) sampleEntries

  -- Query all entries
  putStrLn "All feed entries:"
  entries <- getAllFeedEntries conn
  mapM_ print entries

  -- Query entries by title
  putStrLn "\nEntries with 'Haskell' in title:"
  haskellEntries <- getFeedEntriesByTitle conn "Haskell"
  mapM_ print haskellEntries

  -- Update an entry
  let updatedEntry = (head sampleEntries) {content = "Updated content about Haskell"}
  updateFeedEntry conn "Haskell is great" updatedEntry

  -- Verify update
  putStrLn "\nAfter update:"
  updatedEntries <- getFeedEntriesByTitle conn "Haskell"
  mapM_ print updatedEntries

  -- Clean up (optional)
  deleteFeedEntry conn "Functional Programming"

  -- Close connection
  close conn