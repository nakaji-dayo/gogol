{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}

{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds      #-}
{-# OPTIONS_GHC -fno-warn-unused-imports    #-}

-- |
-- Module      : Network.Google.Resource.DFAReporting.FloodlightActivities.Generatetag
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generates a tag for a floodlight activity.
--
-- /See:/ <https://developers.google.com/doubleclick-advertisers/ DCM/DFA Reporting And Trafficking API Reference> for @dfareporting.floodlightActivities.generatetag@.
module Network.Google.Resource.DFAReporting.FloodlightActivities.Generatetag
    (
    -- * REST Resource
      FloodlightActivitiesGeneratetagResource

    -- * Creating a Request
    , floodlightActivitiesGeneratetag
    , FloodlightActivitiesGeneratetag

    -- * Request Lenses
    , fagFloodlightActivityId
    , fagProFileId
    , fagFields
    ) where

import Network.Google.DFAReporting.Types
import Network.Google.Prelude

-- | A resource alias for @dfareporting.floodlightActivities.generatetag@ method which the
-- 'FloodlightActivitiesGeneratetag' request conforms to.
type FloodlightActivitiesGeneratetagResource =
     "dfareporting" :>
       "v3.0" :>
         "userprofiles" :>
           Capture "profileId" (Textual Int64) :>
             "floodlightActivities" :>
               "generatetag" :>
                 QueryParam "floodlightActivityId" (Textual Int64) :>
                   QueryParam "fields" Text :>
                     QueryParam "alt" AltJSON :>
                       Post '[JSON] FloodlightActivitiesGenerateTagResponse

-- | Generates a tag for a floodlight activity.
--
-- /See:/ 'floodlightActivitiesGeneratetag' smart constructor.
data FloodlightActivitiesGeneratetag = FloodlightActivitiesGeneratetag'
    { _fagFloodlightActivityId :: !(Maybe (Textual Int64))
    , _fagProFileId :: !(Textual Int64)
    , _fagFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'FloodlightActivitiesGeneratetag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fagFloodlightActivityId'
--
-- * 'fagProFileId'
--
-- * 'fagFields'
floodlightActivitiesGeneratetag
    :: Int64 -- ^ 'fagProFileId'
    -> FloodlightActivitiesGeneratetag
floodlightActivitiesGeneratetag pFagProFileId_ = 
    FloodlightActivitiesGeneratetag'
    { _fagFloodlightActivityId = Nothing
    , _fagProFileId = _Coerce # pFagProFileId_
    , _fagFields = Nothing
    }

-- | Floodlight activity ID for which we want to generate a tag.
fagFloodlightActivityId :: Lens' FloodlightActivitiesGeneratetag (Maybe Int64)
fagFloodlightActivityId
  = lens _fagFloodlightActivityId
      (\ s a -> s{_fagFloodlightActivityId = a})
      . mapping _Coerce

-- | User profile ID associated with this request.
fagProFileId :: Lens' FloodlightActivitiesGeneratetag Int64
fagProFileId
  = lens _fagProFileId (\ s a -> s{_fagProFileId = a})
      . _Coerce

-- | Selector specifying which fields to include in a partial response.
fagFields :: Lens' FloodlightActivitiesGeneratetag (Maybe Text)
fagFields
  = lens _fagFields (\ s a -> s{_fagFields = a})

instance GoogleRequest
         FloodlightActivitiesGeneratetag where
        type Rs FloodlightActivitiesGeneratetag =
             FloodlightActivitiesGenerateTagResponse
        type Scopes FloodlightActivitiesGeneratetag =
             '["https://www.googleapis.com/auth/dfatrafficking"]
        requestClient FloodlightActivitiesGeneratetag'{..}
          = go _fagProFileId _fagFloodlightActivityId
              _fagFields
              (Just AltJSON)
              dFAReportingService
          where go
                  = buildClient
                      (Proxy ::
                         Proxy FloodlightActivitiesGeneratetagResource)
                      mempty
