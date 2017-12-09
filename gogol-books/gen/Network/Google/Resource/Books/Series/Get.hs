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
-- Module      : Network.Google.Resource.Books.Series.Get
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns Series metadata for the given series ids.
--
-- /See:/ <https://developers.google.com/books/docs/v1/getting_started Books API Reference> for @books.series.get@.
module Network.Google.Resource.Books.Series.Get
    (
    -- * REST Resource
      SeriesGetResource

    -- * Creating a Request
    , seriesGet
    , SeriesGet

    -- * Request Lenses
    , sgFields
    , sgSeriesId
    ) where

import Network.Google.Books.Types
import Network.Google.Prelude

-- | A resource alias for @books.series.get@ method which the
-- 'SeriesGet' request conforms to.
type SeriesGetResource =
     "books" :>
       "v1" :>
         "series" :>
           "get" :>
             QueryParams "series_id" Text :>
               QueryParam "fields" Text :>
                 QueryParam "alt" AltJSON :> Get '[JSON] Series

-- | Returns Series metadata for the given series ids.
--
-- /See:/ 'seriesGet' smart constructor.
data SeriesGet = SeriesGet'
    { _sgFields :: !(Maybe Text)
    , _sgSeriesId :: ![Text]
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'SeriesGet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sgFields'
--
-- * 'sgSeriesId'
seriesGet
    :: [Text] -- ^ 'sgSeriesId'
    -> SeriesGet
seriesGet pSgSeriesId_ = 
    SeriesGet'
    { _sgFields = Nothing
    , _sgSeriesId = _Coerce # pSgSeriesId_
    }

-- | Selector specifying which fields to include in a partial response.
sgFields :: Lens' SeriesGet (Maybe Text)
sgFields = lens _sgFields (\ s a -> s{_sgFields = a})

-- | String that identifies the series
sgSeriesId :: Lens' SeriesGet [Text]
sgSeriesId
  = lens _sgSeriesId (\ s a -> s{_sgSeriesId = a}) .
      _Coerce

instance GoogleRequest SeriesGet where
        type Rs SeriesGet = Series
        type Scopes SeriesGet =
             '["https://www.googleapis.com/auth/books"]
        requestClient SeriesGet'{..}
          = go _sgSeriesId _sgFields (Just AltJSON)
              booksService
          where go
                  = buildClient (Proxy :: Proxy SeriesGetResource)
                      mempty
