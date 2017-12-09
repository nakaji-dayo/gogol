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
-- Module      : Network.Google.Resource.Analytics.Management.RemarketingAudience.Delete
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete a remarketing audience.
--
-- /See:/ <https://developers.google.com/analytics/ Google Analytics API Reference> for @analytics.management.remarketingAudience.delete@.
module Network.Google.Resource.Analytics.Management.RemarketingAudience.Delete
    (
    -- * REST Resource
      ManagementRemarketingAudienceDeleteResource

    -- * Creating a Request
    , managementRemarketingAudienceDelete
    , ManagementRemarketingAudienceDelete

    -- * Request Lenses
    , mradWebPropertyId
    , mradAccountId
    , mradRemarketingAudienceId
    , mradFields
    ) where

import Network.Google.Analytics.Types
import Network.Google.Prelude

-- | A resource alias for @analytics.management.remarketingAudience.delete@ method which the
-- 'ManagementRemarketingAudienceDelete' request conforms to.
type ManagementRemarketingAudienceDeleteResource =
     "analytics" :>
       "v3" :>
         "management" :>
           "accounts" :>
             Capture "accountId" Text :>
               "webproperties" :>
                 Capture "webPropertyId" Text :>
                   "remarketingAudiences" :>
                     Capture "remarketingAudienceId" Text :>
                       QueryParam "fields" Text :>
                         QueryParam "alt" AltJSON :> Delete '[JSON] ()

-- | Delete a remarketing audience.
--
-- /See:/ 'managementRemarketingAudienceDelete' smart constructor.
data ManagementRemarketingAudienceDelete = ManagementRemarketingAudienceDelete'
    { _mradWebPropertyId :: !Text
    , _mradAccountId :: !Text
    , _mradRemarketingAudienceId :: !Text
    , _mradFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ManagementRemarketingAudienceDelete' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mradWebPropertyId'
--
-- * 'mradAccountId'
--
-- * 'mradRemarketingAudienceId'
--
-- * 'mradFields'
managementRemarketingAudienceDelete
    :: Text -- ^ 'mradWebPropertyId'
    -> Text -- ^ 'mradAccountId'
    -> Text -- ^ 'mradRemarketingAudienceId'
    -> ManagementRemarketingAudienceDelete
managementRemarketingAudienceDelete pMradWebPropertyId_ pMradAccountId_ pMradRemarketingAudienceId_ = 
    ManagementRemarketingAudienceDelete'
    { _mradWebPropertyId = pMradWebPropertyId_
    , _mradAccountId = pMradAccountId_
    , _mradRemarketingAudienceId = pMradRemarketingAudienceId_
    , _mradFields = Nothing
    }

-- | Web property ID to which the remarketing audience belongs.
mradWebPropertyId :: Lens' ManagementRemarketingAudienceDelete Text
mradWebPropertyId
  = lens _mradWebPropertyId
      (\ s a -> s{_mradWebPropertyId = a})

-- | Account ID to which the remarketing audience belongs.
mradAccountId :: Lens' ManagementRemarketingAudienceDelete Text
mradAccountId
  = lens _mradAccountId
      (\ s a -> s{_mradAccountId = a})

-- | The ID of the remarketing audience to delete.
mradRemarketingAudienceId :: Lens' ManagementRemarketingAudienceDelete Text
mradRemarketingAudienceId
  = lens _mradRemarketingAudienceId
      (\ s a -> s{_mradRemarketingAudienceId = a})

-- | Selector specifying which fields to include in a partial response.
mradFields :: Lens' ManagementRemarketingAudienceDelete (Maybe Text)
mradFields
  = lens _mradFields (\ s a -> s{_mradFields = a})

instance GoogleRequest
         ManagementRemarketingAudienceDelete where
        type Rs ManagementRemarketingAudienceDelete = ()
        type Scopes ManagementRemarketingAudienceDelete =
             '["https://www.googleapis.com/auth/analytics.edit"]
        requestClient
          ManagementRemarketingAudienceDelete'{..}
          = go _mradAccountId _mradWebPropertyId
              _mradRemarketingAudienceId
              _mradFields
              (Just AltJSON)
              analyticsService
          where go
                  = buildClient
                      (Proxy ::
                         Proxy ManagementRemarketingAudienceDeleteResource)
                      mempty
