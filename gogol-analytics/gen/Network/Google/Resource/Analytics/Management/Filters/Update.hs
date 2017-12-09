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
-- Module      : Network.Google.Resource.Analytics.Management.Filters.Update
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing filter.
--
-- /See:/ <https://developers.google.com/analytics/ Google Analytics API Reference> for @analytics.management.filters.update@.
module Network.Google.Resource.Analytics.Management.Filters.Update
    (
    -- * REST Resource
      ManagementFiltersUpdateResource

    -- * Creating a Request
    , managementFiltersUpdate
    , ManagementFiltersUpdate

    -- * Request Lenses
    , mfuFilterId
    , mfuPayload
    , mfuAccountId
    , mfuFields
    ) where

import Network.Google.Analytics.Types
import Network.Google.Prelude

-- | A resource alias for @analytics.management.filters.update@ method which the
-- 'ManagementFiltersUpdate' request conforms to.
type ManagementFiltersUpdateResource =
     "analytics" :>
       "v3" :>
         "management" :>
           "accounts" :>
             Capture "accountId" Text :>
               "filters" :>
                 Capture "filterId" Text :>
                   QueryParam "fields" Text :>
                     QueryParam "alt" AltJSON :>
                       ReqBody '[JSON] Filter :> Put '[JSON] Filter

-- | Updates an existing filter.
--
-- /See:/ 'managementFiltersUpdate' smart constructor.
data ManagementFiltersUpdate = ManagementFiltersUpdate'
    { _mfuFilterId :: !Text
    , _mfuPayload :: !Filter
    , _mfuAccountId :: !Text
    , _mfuFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ManagementFiltersUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mfuFilterId'
--
-- * 'mfuPayload'
--
-- * 'mfuAccountId'
--
-- * 'mfuFields'
managementFiltersUpdate
    :: Text -- ^ 'mfuFilterId'
    -> Filter -- ^ 'mfuPayload'
    -> Text -- ^ 'mfuAccountId'
    -> ManagementFiltersUpdate
managementFiltersUpdate pMfuFilterId_ pMfuPayload_ pMfuAccountId_ = 
    ManagementFiltersUpdate'
    { _mfuFilterId = pMfuFilterId_
    , _mfuPayload = pMfuPayload_
    , _mfuAccountId = pMfuAccountId_
    , _mfuFields = Nothing
    }

-- | ID of the filter to be updated.
mfuFilterId :: Lens' ManagementFiltersUpdate Text
mfuFilterId
  = lens _mfuFilterId (\ s a -> s{_mfuFilterId = a})

-- | Multipart request metadata.
mfuPayload :: Lens' ManagementFiltersUpdate Filter
mfuPayload
  = lens _mfuPayload (\ s a -> s{_mfuPayload = a})

-- | Account ID to which the filter belongs.
mfuAccountId :: Lens' ManagementFiltersUpdate Text
mfuAccountId
  = lens _mfuAccountId (\ s a -> s{_mfuAccountId = a})

-- | Selector specifying which fields to include in a partial response.
mfuFields :: Lens' ManagementFiltersUpdate (Maybe Text)
mfuFields
  = lens _mfuFields (\ s a -> s{_mfuFields = a})

instance GoogleRequest ManagementFiltersUpdate where
        type Rs ManagementFiltersUpdate = Filter
        type Scopes ManagementFiltersUpdate =
             '["https://www.googleapis.com/auth/analytics.edit"]
        requestClient ManagementFiltersUpdate'{..}
          = go _mfuAccountId _mfuFilterId _mfuFields
              (Just AltJSON)
              _mfuPayload
              analyticsService
          where go
                  = buildClient
                      (Proxy :: Proxy ManagementFiltersUpdateResource)
                      mempty
