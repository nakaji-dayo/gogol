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
-- Module      : Network.Google.Resource.Mirror.Contacts.Patch
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a contact in place. This method supports patch semantics.
--
-- /See:/ <https://developers.google.com/glass Google Mirror API Reference> for @mirror.contacts.patch@.
module Network.Google.Resource.Mirror.Contacts.Patch
    (
    -- * REST Resource
      ContactsPatchResource

    -- * Creating a Request
    , contactsPatch
    , ContactsPatch

    -- * Request Lenses
    , cpPayload
    , cpId
    , cpFields
    ) where

import Network.Google.Mirror.Types
import Network.Google.Prelude

-- | A resource alias for @mirror.contacts.patch@ method which the
-- 'ContactsPatch' request conforms to.
type ContactsPatchResource =
     "mirror" :>
       "v1" :>
         "contacts" :>
           Capture "id" Text :>
             QueryParam "fields" Text :>
               QueryParam "alt" AltJSON :>
                 ReqBody '[JSON] Contact :> Patch '[JSON] Contact

-- | Updates a contact in place. This method supports patch semantics.
--
-- /See:/ 'contactsPatch' smart constructor.
data ContactsPatch = ContactsPatch'
    { _cpPayload :: !Contact
    , _cpId :: !Text
    , _cpFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ContactsPatch' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpPayload'
--
-- * 'cpId'
--
-- * 'cpFields'
contactsPatch
    :: Contact -- ^ 'cpPayload'
    -> Text -- ^ 'cpId'
    -> ContactsPatch
contactsPatch pCpPayload_ pCpId_ = 
    ContactsPatch'
    { _cpPayload = pCpPayload_
    , _cpId = pCpId_
    , _cpFields = Nothing
    }

-- | Multipart request metadata.
cpPayload :: Lens' ContactsPatch Contact
cpPayload
  = lens _cpPayload (\ s a -> s{_cpPayload = a})

-- | The ID of the contact.
cpId :: Lens' ContactsPatch Text
cpId = lens _cpId (\ s a -> s{_cpId = a})

-- | Selector specifying which fields to include in a partial response.
cpFields :: Lens' ContactsPatch (Maybe Text)
cpFields = lens _cpFields (\ s a -> s{_cpFields = a})

instance GoogleRequest ContactsPatch where
        type Rs ContactsPatch = Contact
        type Scopes ContactsPatch =
             '["https://www.googleapis.com/auth/glass.timeline"]
        requestClient ContactsPatch'{..}
          = go _cpId _cpFields (Just AltJSON) _cpPayload
              mirrorService
          where go
                  = buildClient (Proxy :: Proxy ContactsPatchResource)
                      mempty
