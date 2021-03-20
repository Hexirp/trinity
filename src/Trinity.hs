module Trinity where
  import Prelude

  import qualified Data.Set as Set

  import qualified Data.ByteString as Byte

  import qualified Data.Time as Time

  import qualified Data.UUID as UUID
  import qualified Data.UUID.V4 as UUID

  newtype ID = ID UUID.UUID

  generate_id :: IO ID
  generate_id = ID <$> UUID.nextRandom

  newtype Time = Time Time.UTCTime

  get_current_time :: IO Time
  get_current_time = Time <$> Time.getCurrentTime

  newtype Object = Object Byte.ByteString

  data Triple
    =
      Triple
        {
          triple_id :: !ID,
          triple_subject :: !ID,
          triple_predicate :: !ID,
          triple_object :: !Object,
          triple_time :: !Time,
          triple_author :: !ID
        }

  data Model
    =
      Model
        {
          model_id_set :: !(Set.Set ID),
          model_triple_set :: !(Set.Set Triple)
        }

  generate_initial_model :: IO Model
  generate_initial_model = do
    system_user_id <- generate_id
    adding_operation_id <- generate_id
    return undefined
