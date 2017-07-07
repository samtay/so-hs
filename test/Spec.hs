import Test.Hspec

main :: IO ()
main = hspec $ do

  describe "Google Scraper" $ do
    it "parses english.meta question links" $
      pending
    it "fails gracefully on botched attempt" $
      pending

  describe "StackExchange API JSON" $ do
    it "parses json correctly" $
      pending
    it "filters unanswered questions" $
      pending
    it "fails gracefully on botched attempt" $
      pending

  describe "Configuration" $ do
    it "handles bad yaml configuration" $
      pending
    it "reads site configuration properly" $
      pending
    it "always allows stackoverflow search" $
      pending
    it "handles bad custom ui command gracefully" $
      pending
