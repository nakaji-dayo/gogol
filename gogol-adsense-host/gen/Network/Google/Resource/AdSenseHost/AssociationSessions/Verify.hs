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
-- Module      : Network.Google.Resource.AdSenseHost.AssociationSessions.Verify
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Verify an association session after the association callback returns
-- from AdSense signup.
--
-- /See:/ <https://developers.google.com/adsense/host/ AdSense Host API Reference> for @adsensehost.associationsessions.verify@.
module Network.Google.Resource.AdSenseHost.AssociationSessions.Verify
    (
    -- * REST Resource
      AssociationSessionsVerifyResource

    -- * Creating a Request
    , associationSessionsVerify
    , AssociationSessionsVerify

    -- * Request Lenses
    , asvToken
    , asvFields
    ) where

import Network.Google.AdSenseHost.Types
import Network.Google.Prelude

-- | A resource alias for @adsensehost.associationsessions.verify@ method which the
-- 'AssociationSessionsVerify' request conforms to.
type AssociationSessionsVerifyResource =
     "adsensehost" :>
       "v4.1" :>
         "associationsessions" :>
           "verify" :>
             QueryParam "token" Text :>
               QueryParam "fields" Text :>
                 QueryParam "alt" AltJSON :>
                   Get '[JSON] AssociationSession

-- | Verify an association session after the association callback returns
-- from AdSense signup.
--
-- /See:/ 'associationSessionsVerify' smart constructor.
data AssociationSessionsVerify = AssociationSessionsVerify'
    { _asvToken :: !Text
    , _asvFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'AssociationSessionsVerify' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asvToken'
--
-- * 'asvFields'
associationSessionsVerify
    :: Text -- ^ 'asvToken'
    -> AssociationSessionsVerify
associationSessionsVerify pAsvToken_ = 
    AssociationSessionsVerify'
    { _asvToken = pAsvToken_
    , _asvFields = Nothing
    }

-- | The token returned to the association callback URL.
asvToken :: Lens' AssociationSessionsVerify Text
asvToken = lens _asvToken (\ s a -> s{_asvToken = a})

-- | Selector specifying which fields to include in a partial response.
asvFields :: Lens' AssociationSessionsVerify (Maybe Text)
asvFields
  = lens _asvFields (\ s a -> s{_asvFields = a})

instance GoogleRequest AssociationSessionsVerify
         where
        type Rs AssociationSessionsVerify =
             AssociationSession
        type Scopes AssociationSessionsVerify =
             '["https://www.googleapis.com/auth/adsensehost"]
        requestClient AssociationSessionsVerify'{..}
          = go (Just _asvToken) _asvFields (Just AltJSON)
              adSenseHostService
          where go
                  = buildClient
                      (Proxy :: Proxy AssociationSessionsVerifyResource)
                      mempty
