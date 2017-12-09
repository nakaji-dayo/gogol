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
-- Module      : Network.Google.Resource.Mirror.Contacts.Delete
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a contact.
--
-- /See:/ <https://developers.google.com/glass Google Mirror API Reference> for @mirror.contacts.delete@.
module Network.Google.Resource.Mirror.Contacts.Delete
    (
    -- * REST Resource
      ContactsDeleteResource

    -- * Creating a Request
    , contactsDelete
    , ContactsDelete

    -- * Request Lenses
    , cdId
    , cdFields
    ) where

import Network.Google.Mirror.Types
import Network.Google.Prelude

-- | A resource alias for @mirror.contacts.delete@ method which the
-- 'ContactsDelete' request conforms to.
type ContactsDeleteResource =
     "mirror" :>
       "v1" :>
         "contacts" :>
           Capture "id" Text :>
             QueryParam "fields" Text :>
               QueryParam "alt" AltJSON :> Delete '[JSON] ()

-- | Deletes a contact.
--
-- /See:/ 'contactsDelete' smart constructor.
data ContactsDelete = ContactsDelete'
    { _cdId :: !Text
    , _cdFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ContactsDelete' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdId'
--
-- * 'cdFields'
contactsDelete
    :: Text -- ^ 'cdId'
    -> ContactsDelete
contactsDelete pCdId_ = 
    ContactsDelete'
    { _cdId = pCdId_
    , _cdFields = Nothing
    }

-- | The ID of the contact.
cdId :: Lens' ContactsDelete Text
cdId = lens _cdId (\ s a -> s{_cdId = a})

-- | Selector specifying which fields to include in a partial response.
cdFields :: Lens' ContactsDelete (Maybe Text)
cdFields = lens _cdFields (\ s a -> s{_cdFields = a})

instance GoogleRequest ContactsDelete where
        type Rs ContactsDelete = ()
        type Scopes ContactsDelete =
             '["https://www.googleapis.com/auth/glass.timeline"]
        requestClient ContactsDelete'{..}
          = go _cdId _cdFields (Just AltJSON) mirrorService
          where go
                  = buildClient (Proxy :: Proxy ContactsDeleteResource)
                      mempty
