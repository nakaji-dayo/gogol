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
-- Module      : Network.Google.Resource.DFAReporting.CampaignCreativeAssociations.List
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the list of creative IDs associated with the specified
-- campaign. This method supports paging.
--
-- /See:/ <https://developers.google.com/doubleclick-advertisers/ DCM/DFA Reporting And Trafficking API Reference> for @dfareporting.campaignCreativeAssociations.list@.
module Network.Google.Resource.DFAReporting.CampaignCreativeAssociations.List
    (
    -- * REST Resource
      CampaignCreativeAssociationsListResource

    -- * Creating a Request
    , campaignCreativeAssociationsList
    , CampaignCreativeAssociationsList

    -- * Request Lenses
    , ccalCampaignId
    , ccalProFileId
    , ccalSortOrder
    , ccalPageToken
    , ccalMaxResults
    , ccalFields
    ) where

import Network.Google.DFAReporting.Types
import Network.Google.Prelude

-- | A resource alias for @dfareporting.campaignCreativeAssociations.list@ method which the
-- 'CampaignCreativeAssociationsList' request conforms to.
type CampaignCreativeAssociationsListResource =
     "dfareporting" :>
       "v3.0" :>
         "userprofiles" :>
           Capture "profileId" (Textual Int64) :>
             "campaigns" :>
               Capture "campaignId" (Textual Int64) :>
                 "campaignCreativeAssociations" :>
                   QueryParam "sortOrder"
                     CampaignCreativeAssociationsListSortOrder
                     :>
                     QueryParam "pageToken" Text :>
                       QueryParam "maxResults" (Textual Int32) :>
                         QueryParam "fields" Text :>
                           QueryParam "alt" AltJSON :>
                             Get '[JSON]
                               CampaignCreativeAssociationsListResponse

-- | Retrieves the list of creative IDs associated with the specified
-- campaign. This method supports paging.
--
-- /See:/ 'campaignCreativeAssociationsList' smart constructor.
data CampaignCreativeAssociationsList = CampaignCreativeAssociationsList'
    { _ccalCampaignId :: !(Textual Int64)
    , _ccalProFileId :: !(Textual Int64)
    , _ccalSortOrder :: !CampaignCreativeAssociationsListSortOrder
    , _ccalPageToken :: !(Maybe Text)
    , _ccalMaxResults :: !(Textual Int32)
    , _ccalFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'CampaignCreativeAssociationsList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccalCampaignId'
--
-- * 'ccalProFileId'
--
-- * 'ccalSortOrder'
--
-- * 'ccalPageToken'
--
-- * 'ccalMaxResults'
--
-- * 'ccalFields'
campaignCreativeAssociationsList
    :: Int64 -- ^ 'ccalCampaignId'
    -> Int64 -- ^ 'ccalProFileId'
    -> CampaignCreativeAssociationsList
campaignCreativeAssociationsList pCcalCampaignId_ pCcalProFileId_ = 
    CampaignCreativeAssociationsList'
    { _ccalCampaignId = _Coerce # pCcalCampaignId_
    , _ccalProFileId = _Coerce # pCcalProFileId_
    , _ccalSortOrder = CCALSOAscending
    , _ccalPageToken = Nothing
    , _ccalMaxResults = 1000
    , _ccalFields = Nothing
    }

-- | Campaign ID in this association.
ccalCampaignId :: Lens' CampaignCreativeAssociationsList Int64
ccalCampaignId
  = lens _ccalCampaignId
      (\ s a -> s{_ccalCampaignId = a})
      . _Coerce

-- | User profile ID associated with this request.
ccalProFileId :: Lens' CampaignCreativeAssociationsList Int64
ccalProFileId
  = lens _ccalProFileId
      (\ s a -> s{_ccalProFileId = a})
      . _Coerce

-- | Order of sorted results.
ccalSortOrder :: Lens' CampaignCreativeAssociationsList CampaignCreativeAssociationsListSortOrder
ccalSortOrder
  = lens _ccalSortOrder
      (\ s a -> s{_ccalSortOrder = a})

-- | Value of the nextPageToken from the previous result page.
ccalPageToken :: Lens' CampaignCreativeAssociationsList (Maybe Text)
ccalPageToken
  = lens _ccalPageToken
      (\ s a -> s{_ccalPageToken = a})

-- | Maximum number of results to return.
ccalMaxResults :: Lens' CampaignCreativeAssociationsList Int32
ccalMaxResults
  = lens _ccalMaxResults
      (\ s a -> s{_ccalMaxResults = a})
      . _Coerce

-- | Selector specifying which fields to include in a partial response.
ccalFields :: Lens' CampaignCreativeAssociationsList (Maybe Text)
ccalFields
  = lens _ccalFields (\ s a -> s{_ccalFields = a})

instance GoogleRequest
         CampaignCreativeAssociationsList where
        type Rs CampaignCreativeAssociationsList =
             CampaignCreativeAssociationsListResponse
        type Scopes CampaignCreativeAssociationsList =
             '["https://www.googleapis.com/auth/dfatrafficking"]
        requestClient CampaignCreativeAssociationsList'{..}
          = go _ccalProFileId _ccalCampaignId
              (Just _ccalSortOrder)
              _ccalPageToken
              (Just _ccalMaxResults)
              _ccalFields
              (Just AltJSON)
              dFAReportingService
          where go
                  = buildClient
                      (Proxy ::
                         Proxy CampaignCreativeAssociationsListResource)
                      mempty
