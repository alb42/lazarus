{%MainUnit ../lclintf.pas}

// Open a given URL with the default browser
function OpenURL(AURL: String): Boolean;
begin
  REsult := False;
end;

// Open a document with the default application associated with it in the system
function OpenDocument(APath: String): Boolean;
begin
  Result := False;
end;
