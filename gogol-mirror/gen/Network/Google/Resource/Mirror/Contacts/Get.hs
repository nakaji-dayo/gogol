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
-- Module      : Network.Google.Resource.Mirror.Contacts.Get
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a single contact by ID.
--
-- /See:/ <https://developers.google.com/glass Google Mirror API Reference> for @mirror.contacts.get@.
module Network.Google.Resource.Mirror.Contacts.Get
    (
    -- * REST Resource
      ContactsGetResource

    -- * Creating a Request
    , contactsGet
    , ContactsGet

    -- * Request Lenses
    , cgId
    , cgFields
    ) where

import Network.Google.Mirror.Types
import Network.Google.Prelude

-- | A resource alias for @mirror.contacts.get@ method which the
-- 'ContactsGet' request conforms to.
type ContactsGetResource =
     "mirror" :>
       "v1" :>
         "contacts" :>
           Capture "id" Text :>
             QueryParam "fields" Text :>
               QueryParam "alt" AltJSON :> Get '[JSON] Contact

-- | Gets a single contact by ID.
--
-- /See:/ 'contactsGet' smart constructor.
data ContactsGet = ContactsGet'
    { _cgId :: !Text
    , _cgFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ContactsGet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cgId'
--
-- * 'cgFields'
contactsGet
    :: Text -- ^ 'cgId'
    -> ContactsGet
contactsGet pCgId_ = 
    ContactsGet'
    { _cgId = pCgId_
    , _cgFields = Nothing
    }

-- | The ID of the contact.
cgId :: Lens' ContactsGet Text
cgId = lens _cgId (\ s a -> s{_cgId = a})

-- | Selector specifying which fields to include in a partial response.
cgFields :: Lens' ContactsGet (Maybe Text)
cgFields = lens _cgFields (\ s a -> s{_cgFields = a})

instance GoogleRequest ContactsGet where
        type Rs ContactsGet = Contact
        type Scopes ContactsGet =
             '["https://www.googleapis.com/auth/glass.timeline"]
        requestClient ContactsGet'{..}
          = go _cgId _cgFields (Just AltJSON) mirrorService
          where go
                  = buildClient (Proxy :: Proxy ContactsGetResource)
                      mempty
