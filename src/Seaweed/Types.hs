{- | Types for @CV@ representation.
-}

module Seaweed.Types
       ( Seaweed (..)
       ) where

import Data.Text (Text)
import Data.Time (Day)


{- | Main type that contains all necessary information for @CV@ creation.
-}
data Seaweed = Seaweed
    { -- | Full name.
      seaweedName       :: !Text
      -- | E.g. "Software engineer", "Frontend Developer".
    , seaweedOccupation :: !Text
      -- | The current location (optional field).
    , seaweedLocation   :: !(Maybe Location)
      -- | Mobile phone number (optional field).
    , seaweedPhone      :: !(Maybe Text)
      -- | Email address (optional field).
    , seaweedEmail      :: !(Maybe Text)
      -- | @GitHub@ user name
    , seaweedGitHub     :: !(Maybe Text)
      -- | Work experience.
    , seaweedWork       :: ![Work]
      -- | Education information.
    , seaweedEducation  :: ![Education]
      -- | Languages knowledge
    , seaweedLanguages  :: ![Language]
      -- | Different technical skills
    , seaweedSkills     :: ![Text]
      -- | Other information: hobbies, advantages etc.
    , seaweedOther      :: !(Maybe Text)
    } deriving (Show, Eq)

data Location = Location
    { locationCountry :: Text
    , locationCity    :: Text
    } deriving (Show, Eq)

data Work = Work
    { workPosition    :: !Text
    , workCompany     :: !Company
    , workStart       :: !Day  -- ^ From period in @YYYY-MM-DD@ format.
    , workEnd         :: !(Maybe Day)  -- ^ To period in @YYYY-MM-DD@ format (optional).
    , workLocation    :: !(Maybe Location)
    , workDescription :: !Text
    } deriving (Show, Eq)

-- | Information about the company.
data Company = Company
    { companyName :: !Text
    , companyLink :: !Text
    } deriving (Show, Eq)

-- | Information about the schools, universities, courses etc.
data Education = Education
    { educationDegree      :: !Text
    , educationUniversity  :: !Text
    , educationLocation    :: !(Maybe Location)
    , educationStartYear   :: !Int
    , educationEndYear     :: !Int
    , educationFaculty     :: !(Maybe Text)
    , educationDescription :: !(Maybe Text)
    } deriving (Show, Eq)

-- | Languages level information.
data Language = Language
    { languageName  :: !Text
    , languageLevel :: !Text
    } deriving (Show, Eq)
