{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}

module Trinity where
  import Prelude

  import GHC.Generics

  import qualified Data.Set as Set

  import qualified Data.ByteString as Byte

  import qualified Data.Time as Time

  import qualified Data.UUID as UUID
  import qualified Data.UUID.V4 as UUID

  newtype ID = ID UUID.UUID
    deriving stock Eq
    deriving stock Ord
    deriving stock Show
    deriving stock Read
    deriving stock Generic

  generate_id :: IO ID
  generate_id = ID <$> UUID.nextRandom

  newtype Time = Time Time.UTCTime
    deriving stock Eq
    deriving stock Ord
    deriving stock Show
    deriving stock Read
    deriving stock Generic

  get_current_time :: IO Time
  get_current_time = Time <$> Time.getCurrentTime

  newtype Object = Object Byte.ByteString
    deriving stock Eq
    deriving stock Ord
    deriving stock Show
    deriving stock Read
    deriving stock Generic

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
    deriving stock Eq
    deriving stock Ord
    deriving stock Show
    deriving stock Read
    deriving stock Generic

  data Model
    =
      Model
        {
          model_id_set :: !(Set.Set ID),
          model_triple_set :: !(Set.Set Triple)
        }
    deriving stock Eq
    deriving stock Ord
    deriving stock Show
    deriving stock Read
    deriving stock Generic

  generate_initial_model :: IO Model
  generate_initial_model = do
    system_user_id <- generate_id
    adding_operation_id <- generate_id
    return undefined
