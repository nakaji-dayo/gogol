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
-- Module      : Network.Google.Resource.DFAReporting.Placements.Generatetags
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generates tags for a placement.
--
-- /See:/ <https://developers.google.com/doubleclick-advertisers/ DCM/DFA Reporting And Trafficking API Reference> for @dfareporting.placements.generatetags@.
module Network.Google.Resource.DFAReporting.Placements.Generatetags
    (
    -- * REST Resource
      PlacementsGeneratetagsResource

    -- * Creating a Request
    , placementsGeneratetags
    , PlacementsGeneratetags

    -- * Request Lenses
    , pgsTagFormats
    , pgsCampaignId
    , pgsProFileId
    , pgsPlacementIds
    , pgsFields
    ) where

import Network.Google.DFAReporting.Types
import Network.Google.Prelude

-- | A resource alias for @dfareporting.placements.generatetags@ method which the
-- 'PlacementsGeneratetags' request conforms to.
type PlacementsGeneratetagsResource =
     "dfareporting" :>
       "v3.0" :>
         "userprofiles" :>
           Capture "profileId" (Textual Int64) :>
             "placements" :>
               "generatetags" :>
                 QueryParams "tagFormats"
                   PlacementsGeneratetagsTagFormats
                   :>
                   QueryParam "campaignId" (Textual Int64) :>
                     QueryParams "placementIds" (Textual Int64) :>
                       QueryParam "fields" Text :>
                         QueryParam "alt" AltJSON :>
                           Post '[JSON] PlacementsGenerateTagsResponse

-- | Generates tags for a placement.
--
-- /See:/ 'placementsGeneratetags' smart constructor.
data PlacementsGeneratetags = PlacementsGeneratetags'
    { _pgsTagFormats :: !(Maybe [PlacementsGeneratetagsTagFormats])
    , _pgsCampaignId :: !(Maybe (Textual Int64))
    , _pgsProFileId :: !(Textual Int64)
    , _pgsPlacementIds :: !(Maybe [Textual Int64])
    , _pgsFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'PlacementsGeneratetags' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pgsTagFormats'
--
-- * 'pgsCampaignId'
--
-- * 'pgsProFileId'
--
-- * 'pgsPlacementIds'
--
-- * 'pgsFields'
placementsGeneratetags
    :: Int64 -- ^ 'pgsProFileId'
    -> PlacementsGeneratetags
placementsGeneratetags pPgsProFileId_ = 
    PlacementsGeneratetags'
    { _pgsTagFormats = Nothing
    , _pgsCampaignId = Nothing
    , _pgsProFileId = _Coerce # pPgsProFileId_
    , _pgsPlacementIds = Nothing
    , _pgsFields = Nothing
    }

-- | Tag formats to generate for these placements. Note:
-- PLACEMENT_TAG_STANDARD can only be generated for 1x1 placements.
pgsTagFormats :: Lens' PlacementsGeneratetags [PlacementsGeneratetagsTagFormats]
pgsTagFormats
  = lens _pgsTagFormats
      (\ s a -> s{_pgsTagFormats = a})
      . _Default
      . _Coerce

-- | Generate placements belonging to this campaign. This is a required
-- field.
pgsCampaignId :: Lens' PlacementsGeneratetags (Maybe Int64)
pgsCampaignId
  = lens _pgsCampaignId
      (\ s a -> s{_pgsCampaignId = a})
      . mapping _Coerce

-- | User profile ID associated with this request.
pgsProFileId :: Lens' PlacementsGeneratetags Int64
pgsProFileId
  = lens _pgsProFileId (\ s a -> s{_pgsProFileId = a})
      . _Coerce

-- | Generate tags for these placements.
pgsPlacementIds :: Lens' PlacementsGeneratetags [Int64]
pgsPlacementIds
  = lens _pgsPlacementIds
      (\ s a -> s{_pgsPlacementIds = a})
      . _Default
      . _Coerce

-- | Selector specifying which fields to include in a partial response.
pgsFields :: Lens' PlacementsGeneratetags (Maybe Text)
pgsFields
  = lens _pgsFields (\ s a -> s{_pgsFields = a})

instance GoogleRequest PlacementsGeneratetags where
        type Rs PlacementsGeneratetags =
             PlacementsGenerateTagsResponse
        type Scopes PlacementsGeneratetags =
             '["https://www.googleapis.com/auth/dfatrafficking"]
        requestClient PlacementsGeneratetags'{..}
          = go _pgsProFileId (_pgsTagFormats ^. _Default)
              _pgsCampaignId
              (_pgsPlacementIds ^. _Default)
              _pgsFields
              (Just AltJSON)
              dFAReportingService
          where go
                  = buildClient
                      (Proxy :: Proxy PlacementsGeneratetagsResource)
                      mempty
