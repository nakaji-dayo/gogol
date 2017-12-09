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
-- Module      : Network.Google.Resource.Spectrum.Paws.GetSpectrumBatch
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The Google Spectrum Database does not support batch requests, so this
-- method always yields an UNIMPLEMENTED error.
--
-- /See:/ <http://developers.google.com/spectrum Google Spectrum Database API Reference> for @spectrum.paws.getSpectrumBatch@.
module Network.Google.Resource.Spectrum.Paws.GetSpectrumBatch
    (
    -- * REST Resource
      PawsGetSpectrumBatchResource

    -- * Creating a Request
    , pawsGetSpectrumBatch
    , PawsGetSpectrumBatch

    -- * Request Lenses
    , pgsbPayload
    , pgsbFields
    ) where

import Network.Google.Prelude
import Network.Google.Spectrum.Types

-- | A resource alias for @spectrum.paws.getSpectrumBatch@ method which the
-- 'PawsGetSpectrumBatch' request conforms to.
type PawsGetSpectrumBatchResource =
     "spectrum" :>
       "v1explorer" :>
         "paws" :>
           "getSpectrumBatch" :>
             QueryParam "fields" Text :>
               QueryParam "alt" AltJSON :>
                 ReqBody '[JSON] PawsGetSpectrumBatchRequest :>
                   Post '[JSON] PawsGetSpectrumBatchResponse

-- | The Google Spectrum Database does not support batch requests, so this
-- method always yields an UNIMPLEMENTED error.
--
-- /See:/ 'pawsGetSpectrumBatch' smart constructor.
data PawsGetSpectrumBatch = PawsGetSpectrumBatch'
    { _pgsbPayload :: !PawsGetSpectrumBatchRequest
    , _pgsbFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'PawsGetSpectrumBatch' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pgsbPayload'
--
-- * 'pgsbFields'
pawsGetSpectrumBatch
    :: PawsGetSpectrumBatchRequest -- ^ 'pgsbPayload'
    -> PawsGetSpectrumBatch
pawsGetSpectrumBatch pPgsbPayload_ = 
    PawsGetSpectrumBatch'
    { _pgsbPayload = pPgsbPayload_
    , _pgsbFields = Nothing
    }

-- | Multipart request metadata.
pgsbPayload :: Lens' PawsGetSpectrumBatch PawsGetSpectrumBatchRequest
pgsbPayload
  = lens _pgsbPayload (\ s a -> s{_pgsbPayload = a})

-- | Selector specifying which fields to include in a partial response.
pgsbFields :: Lens' PawsGetSpectrumBatch (Maybe Text)
pgsbFields
  = lens _pgsbFields (\ s a -> s{_pgsbFields = a})

instance GoogleRequest PawsGetSpectrumBatch where
        type Rs PawsGetSpectrumBatch =
             PawsGetSpectrumBatchResponse
        type Scopes PawsGetSpectrumBatch = '[]
        requestClient PawsGetSpectrumBatch'{..}
          = go _pgsbFields (Just AltJSON) _pgsbPayload
              spectrumService
          where go
                  = buildClient
                      (Proxy :: Proxy PawsGetSpectrumBatchResource)
                      mempty
