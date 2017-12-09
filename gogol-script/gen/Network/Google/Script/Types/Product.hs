{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.Google.Script.Types.Product
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.Google.Script.Types.Product where

import Network.Google.Prelude
import Network.Google.Script.Types.Sum

-- | If a \`run\` call succeeds but the script function (or Apps Script
-- itself) throws an exception, the response body\'s error field contains
-- this \`Status\` object.
--
-- /See:/ 'status' smart constructor.
data Status = Status'
    { _sDetails :: !(Maybe [StatusDetailsItem])
    , _sCode :: !(Maybe (Textual Int32))
    , _sMessage :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'Status' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sDetails'
--
-- * 'sCode'
--
-- * 'sMessage'
status
    :: Status
status = 
    Status'
    { _sDetails = Nothing
    , _sCode = Nothing
    , _sMessage = Nothing
    }

-- | An array that contains a single ExecutionError object that provides
-- information about the nature of the error.
sDetails :: Lens' Status [StatusDetailsItem]
sDetails
  = lens _sDetails (\ s a -> s{_sDetails = a}) .
      _Default
      . _Coerce

-- | The status code. For this API, this value either:
--
-- -   3, indicating an \`INVALID_ARGUMENT\` error, or
-- -   1, indicating a \`CANCELLED\` execution.
sCode :: Lens' Status (Maybe Int32)
sCode
  = lens _sCode (\ s a -> s{_sCode = a}) .
      mapping _Coerce

-- | A developer-facing error message, which is in English. Any user-facing
-- error message is localized and sent in the
-- [google.rpc.Status.details](google.rpc.Status.details) field, or
-- localized by the client.
sMessage :: Lens' Status (Maybe Text)
sMessage = lens _sMessage (\ s a -> s{_sMessage = a})

instance FromJSON Status where
        parseJSON
          = withObject "Status"
              (\ o ->
                 Status' <$>
                   (o .:? "details" .!= mempty) <*> (o .:? "code") <*>
                     (o .:? "message"))

instance ToJSON Status where
        toJSON Status'{..}
          = object
              (catMaybes
                 [("details" .=) <$> _sDetails,
                  ("code" .=) <$> _sCode,
                  ("message" .=) <$> _sMessage])

-- | A representation of a execution of an Apps Script function that is
-- started using run. The execution response does not arrive until the
-- function finishes executing. The maximum execution runtime is listed in
-- the [Apps Script quotas
-- guide](\/apps-script\/guides\/services\/quotas#current_limitations).
--
-- After the execution is started, it can have one of four outcomes:
--
-- -   If the script function returns successfully, the response field
--     contains an ExecutionResponse object with the function\'s return
--     value in the object\'s \`result\` field.
-- -   If the script function (or Apps Script itself) throws an exception,
--     the error field contains a Status object. The \`Status\` object\'s
--     \`details\` field contains an array with a single ExecutionError
--     object that provides information about the nature of the error.
-- -   If the execution has not yet completed, the done field is \`false\`
--     and the neither the \`response\` nor \`error\` fields are present.
-- -   If the \`run\` call itself fails (for example, because of a
--     malformed request or an authorization error), the method returns an
--     HTTP response code in the 4XX range with a different format for the
--     response body. Client libraries automatically convert a 4XX response
--     into an exception class.
--
-- /See:/ 'operation' smart constructor.
data Operation = Operation'
    { _oDone :: !(Maybe Bool)
    , _oError :: !(Maybe Status)
    , _oResponse :: !(Maybe OperationResponse)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'Operation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oDone'
--
-- * 'oError'
--
-- * 'oResponse'
operation
    :: Operation
operation = 
    Operation'
    { _oDone = Nothing
    , _oError = Nothing
    , _oResponse = Nothing
    }

-- | This field indicates whether the script execution has completed. A
-- completed execution has a populated \`response\` field containing the
-- ExecutionResponse from function that was executed.
oDone :: Lens' Operation (Maybe Bool)
oDone = lens _oDone (\ s a -> s{_oDone = a})

-- | If a \`run\` call succeeds but the script function (or Apps Script
-- itself) throws an exception, this field contains a Status object. The
-- \`Status\` object\'s \`details\` field contains an array with a single
-- ExecutionError object that provides information about the nature of the
-- error.
oError :: Lens' Operation (Maybe Status)
oError = lens _oError (\ s a -> s{_oError = a})

-- | If the script function returns successfully, this field contains an
-- ExecutionResponse object with the function\'s return value.
oResponse :: Lens' Operation (Maybe OperationResponse)
oResponse
  = lens _oResponse (\ s a -> s{_oResponse = a})

instance FromJSON Operation where
        parseJSON
          = withObject "Operation"
              (\ o ->
                 Operation' <$>
                   (o .:? "done") <*> (o .:? "error") <*>
                     (o .:? "response"))

instance ToJSON Operation where
        toJSON Operation'{..}
          = object
              (catMaybes
                 [("done" .=) <$> _oDone, ("error" .=) <$> _oError,
                  ("response" .=) <$> _oResponse])

-- | A request to run the function in a script. The script is identified by
-- the specified \`script_id\`. Executing a function on a script returns
-- results based on the implementation of the script.
--
-- /See:/ 'executionRequest' smart constructor.
data ExecutionRequest = ExecutionRequest'
    { _erFunction :: !(Maybe Text)
    , _erSessionState :: !(Maybe Text)
    , _erDevMode :: !(Maybe Bool)
    , _erParameters :: !(Maybe [JSONValue])
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ExecutionRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'erFunction'
--
-- * 'erSessionState'
--
-- * 'erDevMode'
--
-- * 'erParameters'
executionRequest
    :: ExecutionRequest
executionRequest = 
    ExecutionRequest'
    { _erFunction = Nothing
    , _erSessionState = Nothing
    , _erDevMode = Nothing
    , _erParameters = Nothing
    }

-- | The name of the function to execute in the given script. The name does
-- not include parentheses or parameters.
erFunction :: Lens' ExecutionRequest (Maybe Text)
erFunction
  = lens _erFunction (\ s a -> s{_erFunction = a})

-- | For Android add-ons only. An ID that represents the user\'s current
-- session in the Android app for Google Docs or Sheets, included as extra
-- data in the
-- [Intent](https:\/\/developer.android.com\/guide\/components\/intents-filters.html)
-- that launches the add-on. When an Android add-on is run with a session
-- state, it gains the privileges of a
-- [bound](https:\/\/developers.google.com\/apps-script\/guides\/bound)
-- script—that is, it can access information like the user\'s current
-- cursor position (in Docs) or selected cell (in Sheets). To retrieve the
-- state, call
-- \`Intent.getStringExtra(\"com.google.android.apps.docs.addons.SessionState\")\`.
-- Optional.
erSessionState :: Lens' ExecutionRequest (Maybe Text)
erSessionState
  = lens _erSessionState
      (\ s a -> s{_erSessionState = a})

-- | If \`true\` and the user is an owner of the script, the script runs at
-- the most recently saved version rather than the version deployed for use
-- with the Apps Script API. Optional; default is \`false\`.
erDevMode :: Lens' ExecutionRequest (Maybe Bool)
erDevMode
  = lens _erDevMode (\ s a -> s{_erDevMode = a})

-- | The parameters to be passed to the function being executed. The object
-- type for each parameter should match the expected type in Apps Script.
-- Parameters cannot be Apps Script-specific object types (such as a
-- \`Document\` or a \`Calendar\`); they can only be primitive types such
-- as \`string\`, \`number\`, \`array\`, \`object\`, or \`boolean\`.
-- Optional.
erParameters :: Lens' ExecutionRequest [JSONValue]
erParameters
  = lens _erParameters (\ s a -> s{_erParameters = a})
      . _Default
      . _Coerce

instance FromJSON ExecutionRequest where
        parseJSON
          = withObject "ExecutionRequest"
              (\ o ->
                 ExecutionRequest' <$>
                   (o .:? "function") <*> (o .:? "sessionState") <*>
                     (o .:? "devMode")
                     <*> (o .:? "parameters" .!= mempty))

instance ToJSON ExecutionRequest where
        toJSON ExecutionRequest'{..}
          = object
              (catMaybes
                 [("function" .=) <$> _erFunction,
                  ("sessionState" .=) <$> _erSessionState,
                  ("devMode" .=) <$> _erDevMode,
                  ("parameters" .=) <$> _erParameters])

--
-- /See:/ 'statusDetailsItem' smart constructor.
newtype StatusDetailsItem = StatusDetailsItem'
    { _sdiAddtional :: HashMap Text JSONValue
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'StatusDetailsItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdiAddtional'
statusDetailsItem
    :: HashMap Text JSONValue -- ^ 'sdiAddtional'
    -> StatusDetailsItem
statusDetailsItem pSdiAddtional_ = 
    StatusDetailsItem'
    { _sdiAddtional = _Coerce # pSdiAddtional_
    }

-- | Properties of the object. Contains field \'type with type URL.
sdiAddtional :: Lens' StatusDetailsItem (HashMap Text JSONValue)
sdiAddtional
  = lens _sdiAddtional (\ s a -> s{_sdiAddtional = a})
      . _Coerce

instance FromJSON StatusDetailsItem where
        parseJSON
          = withObject "StatusDetailsItem"
              (\ o -> StatusDetailsItem' <$> (parseJSONObject o))

instance ToJSON StatusDetailsItem where
        toJSON = toJSON . _sdiAddtional

-- | A stack trace through the script that shows where the execution failed.
--
-- /See:/ 'scriptStackTraceElement' smart constructor.
data ScriptStackTraceElement = ScriptStackTraceElement'
    { _ssteFunction :: !(Maybe Text)
    , _ssteLineNumber :: !(Maybe (Textual Int32))
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ScriptStackTraceElement' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssteFunction'
--
-- * 'ssteLineNumber'
scriptStackTraceElement
    :: ScriptStackTraceElement
scriptStackTraceElement = 
    ScriptStackTraceElement'
    { _ssteFunction = Nothing
    , _ssteLineNumber = Nothing
    }

-- | The name of the function that failed.
ssteFunction :: Lens' ScriptStackTraceElement (Maybe Text)
ssteFunction
  = lens _ssteFunction (\ s a -> s{_ssteFunction = a})

-- | The line number where the script failed.
ssteLineNumber :: Lens' ScriptStackTraceElement (Maybe Int32)
ssteLineNumber
  = lens _ssteLineNumber
      (\ s a -> s{_ssteLineNumber = a})
      . mapping _Coerce

instance FromJSON ScriptStackTraceElement where
        parseJSON
          = withObject "ScriptStackTraceElement"
              (\ o ->
                 ScriptStackTraceElement' <$>
                   (o .:? "function") <*> (o .:? "lineNumber"))

instance ToJSON ScriptStackTraceElement where
        toJSON ScriptStackTraceElement'{..}
          = object
              (catMaybes
                 [("function" .=) <$> _ssteFunction,
                  ("lineNumber" .=) <$> _ssteLineNumber])

-- | An object that provides information about the nature of an error
-- resulting from an attempted execution of a script function using the
-- Apps Script API. If a run call succeeds but the script function (or Apps
-- Script itself) throws an exception, the response body\'s error field
-- contains a Status object. The \`Status\` object\'s \`details\` field
-- contains an array with a single one of these \`ExecutionError\` objects.
--
-- /See:/ 'executionError' smart constructor.
data ExecutionError = ExecutionError'
    { _eeScriptStackTraceElements :: !(Maybe [ScriptStackTraceElement])
    , _eeErrorType :: !(Maybe Text)
    , _eeErrorMessage :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ExecutionError' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eeScriptStackTraceElements'
--
-- * 'eeErrorType'
--
-- * 'eeErrorMessage'
executionError
    :: ExecutionError
executionError = 
    ExecutionError'
    { _eeScriptStackTraceElements = Nothing
    , _eeErrorType = Nothing
    , _eeErrorMessage = Nothing
    }

-- | An array of objects that provide a stack trace through the script to
-- show where the execution failed, with the deepest call first.
eeScriptStackTraceElements :: Lens' ExecutionError [ScriptStackTraceElement]
eeScriptStackTraceElements
  = lens _eeScriptStackTraceElements
      (\ s a -> s{_eeScriptStackTraceElements = a})
      . _Default
      . _Coerce

-- | The error type, for example \`TypeError\` or \`ReferenceError\`. If the
-- error type is unavailable, this field is not included.
eeErrorType :: Lens' ExecutionError (Maybe Text)
eeErrorType
  = lens _eeErrorType (\ s a -> s{_eeErrorType = a})

-- | The error message thrown by Apps Script, usually localized into the
-- user\'s language.
eeErrorMessage :: Lens' ExecutionError (Maybe Text)
eeErrorMessage
  = lens _eeErrorMessage
      (\ s a -> s{_eeErrorMessage = a})

instance FromJSON ExecutionError where
        parseJSON
          = withObject "ExecutionError"
              (\ o ->
                 ExecutionError' <$>
                   (o .:? "scriptStackTraceElements" .!= mempty) <*>
                     (o .:? "errorType")
                     <*> (o .:? "errorMessage"))

instance ToJSON ExecutionError where
        toJSON ExecutionError'{..}
          = object
              (catMaybes
                 [("scriptStackTraceElements" .=) <$>
                    _eeScriptStackTraceElements,
                  ("errorType" .=) <$> _eeErrorType,
                  ("errorMessage" .=) <$> _eeErrorMessage])

-- | If the script function returns successfully, this field contains an
-- ExecutionResponse object with the function\'s return value.
--
-- /See:/ 'operationResponse' smart constructor.
newtype OperationResponse = OperationResponse'
    { _orAddtional :: HashMap Text JSONValue
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'OperationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'orAddtional'
operationResponse
    :: HashMap Text JSONValue -- ^ 'orAddtional'
    -> OperationResponse
operationResponse pOrAddtional_ = 
    OperationResponse'
    { _orAddtional = _Coerce # pOrAddtional_
    }

-- | Properties of the object. Contains field \'type with type URL.
orAddtional :: Lens' OperationResponse (HashMap Text JSONValue)
orAddtional
  = lens _orAddtional (\ s a -> s{_orAddtional = a}) .
      _Coerce

instance FromJSON OperationResponse where
        parseJSON
          = withObject "OperationResponse"
              (\ o -> OperationResponse' <$> (parseJSONObject o))

instance ToJSON OperationResponse where
        toJSON = toJSON . _orAddtional

-- | An object that provides the return value of a function executed using
-- the Apps Script API. If the script function returns successfully, the
-- response body\'s response field contains this \`ExecutionResponse\`
-- object.
--
-- /See:/ 'executionResponse' smart constructor.
newtype ExecutionResponse = ExecutionResponse'
    { _erResult :: Maybe JSONValue
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ExecutionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'erResult'
executionResponse
    :: ExecutionResponse
executionResponse = 
    ExecutionResponse'
    { _erResult = Nothing
    }

-- | The return value of the script function. The type matches the object
-- type returned in Apps Script. Functions called using the Apps Script API
-- cannot return Apps Script-specific objects (such as a \`Document\` or a
-- \`Calendar\`); they can only return primitive types such as a
-- \`string\`, \`number\`, \`array\`, \`object\`, or \`boolean\`.
erResult :: Lens' ExecutionResponse (Maybe JSONValue)
erResult = lens _erResult (\ s a -> s{_erResult = a})

instance FromJSON ExecutionResponse where
        parseJSON
          = withObject "ExecutionResponse"
              (\ o -> ExecutionResponse' <$> (o .:? "result"))

instance ToJSON ExecutionResponse where
        toJSON ExecutionResponse'{..}
          = object (catMaybes [("result" .=) <$> _erResult])
