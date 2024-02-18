{==============================================================================

MIT License

Copyright (c) 2024 Paweł Dmitruk (paweld), https://github.com/paweld

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

==============================================================================}

{
Get current weather and forecast from OpenWeather API
}

unit LPDWeatherU;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, DateUtils, fphttpclient, opensslsockets, httpprotocol, fpjson, jsonparser, fgl;

type

  TWeatherUnits = (wuMetric, wuStandard, wuImperial);
  { metric [temp: Celsius, wind: m/s], standard [temp: Kelvin, wind: m/s], imperial [temp: Fahrenheit, wind: mile/h] }

  TWeatherLang = (wlAf {Afrikaans}, wlAl {Albanian}, wlAr {Arabic}, wlAz {Azerbaijani}, wlBg {Bulgarian}, wlCa {Catalan}, wlCz {Czech}, wlDa {Danish},
    wlDe {German}, wlEl {Greek}, wlEn {English}, wlEs {Spanish}, wlEu {Basque}, wlFa {Persian (Farsi)}, wlFi {Finnish}, wlFr {French}, wlGl {Galician},
    wlHe {Hebrew}, wlHi {Hindi}, wlHr {Croatian}, wlHu {Hungarian}, wlId {Indonesian}, wlIt {Italian}, wlJa {Japanese}, wlKr {Korean}, wlLa {Latvian},
    wlLt {Lithuanian}, wlMk {Macedonian}, wlNo {Norwegian}, wlNl {Dutch}, wlPl {Polish}, wlPt {Portuguese}, wlPt_br {Português Brasil}, wlRo {Romanian},
    wlRu {Russian}, wlSv {Swedish}, wlSk {Slovak}, wlSl {Slovenian}, wlSr {Serbian}, wlTh {Thai}, wlTr {Turkish}, wlUk {Ukrainian}, wlVi {Vietnamese},
    wlZh_cn {Chinese Simplified}, wlZh_tw {Chinese Traditional}, wlZu {Zulu});

  { TWeatherItem }

  TWeatherItem = class
  private
    FPressure: Currency;
    FDate: TDateTime;
    FWeatherIcon: TMemoryStream;
    FDescription: String;
    FTemp: Currency;
    FTempMax: Currency;
    FTempMin: Currency;
    FTempFeelsLike: Currency;
    FWindDirection: String;
    FWindSpeed: Currency;
    FWindDegrees: Currency;
    FWindGust: Currency;
    FVisibility: Currency;
    FHumidity: Currency;
    FSunrise: TTime;
    FSunset: TTime;
    procedure SetWindDegrees(aValue: Currency);
  public
    constructor Create;
    destructor Destroy; override;
    property Date: TDateTime read FDate write FDate;
    property Description: String read FDescription write FDescription;
    property Temp: Currency read FTemp write FTemp;
    property TempFeelsLike: Currency read FTempFeelsLike write FTempFeelsLike;
    property TempMin: Currency read FTempMin write FTempMin;
    property TempMax: Currency read FTempMax write FTempMax;
    property Humidity: Currency read FHumidity write FHumidity;
    property Pressure: Currency read FPressure write FPressure;
    property Visibility: Currency read FVisibility write FVisibility;
    property WindSpeed: Currency read FWindSpeed write FWindSpeed;
    property WindDegrees: Currency read FWindDegrees write SetWindDegrees;
    property WindDirection: String read FWindDirection write FWindDirection;
    property WindGust: Currency read FWindGust write FWindGust;
    property Sunrise: TTime read FSunrise write FSunrise;
    property Sunset: TTime read FSunset write FSunset;
    property WeatherIcon: TMemoryStream read FWeatherIcon write FWeatherIcon;
  end;

  { TWeatherList }

  TWeatherList = class(specialize TFPGMapObject<TDateTime, TWeatherItem>)
  public
    constructor Create;
    function IndexOfDate(aDate: TDatetime): Integer;
  end;

  { TWeather }

  TWeather = class
  private
    FCurrentWeather: TWeatherItem;
    FAPIKey: String;
    FCity: String;
    FLang: TWeatherLang;
    FLastError: String;
    FLatitude: Double;
    FLongitude: Extended;
    FUnits: TWeatherUnits;
    FWeatherForecast: TWeatherList;
    function GetLangCode: String;
    function GetUnitsCode: String;
    procedure Parse(ajo: TJsonObject; var aPogodaItem: TWeatherItem);
    procedure SetLang(AValue: TWeatherLang);
    procedure SetLangCode(AValue: String);
    procedure SetUnits(AValue: TWeatherUnits);
    procedure SetCity(AValue: String);
  public
    constructor Create;
    destructor Destroy; override;
    procedure ClearWeatherInfo;
    function Get(aCity: String; aCountryCode: String = ''; aStateCode: String = ''): Boolean;
    function Get(aLat, aLon: Double): Boolean;
    property APIKey: String read FAPIKey write FAPIKey;
    property CurrentWeather: TWeatherItem read FCurrentWeather;
    property City: String read FCity;
    property Lang: TWeatherLang read FLang write SetLang;
    property LangCode: String read GetLangCode write SetLangCode;
    property Units: TWeatherUnits read FUnits write SetUnits;
    property WeatherForecast: TWeatherList read FWeatherForecast;
    property LastError: String read FLastError;
    property Latitude: Double read FLatitude;
    property Longitude: Extended read FLongitude;
  end;

implementation

{ TWeatherItem }

procedure TWeatherItem.SetWindDegrees(aValue: Currency);
const
  wdarr: array [0..7] of String = ('N', 'NE', 'E', 'SE', 'S', 'SW', 'W', 'NW');
var
  i, idx, range: Integer;
  deg: Currency;
begin
  FWindDegrees := aValue;
  if (FWindDegrees < 0) or (FWindDegrees > 360) then
    raise Exception.Create('Incorrect WindDegrees!');
  deg := FWindDegrees;
  if deg = 0 then
    deg := 1;
  range := 360 div Length(wdarr);
  idx := 0;
  for i := Low(wdarr) to High(wdarr) do
  begin
    if (deg >= ((range * i) - (range div 2))) and (deg <= ((range * (i + 1)) - (range div 2))) then
    begin
      idx := i;
      break;
    end;
  end;
  FWindDirection := wdarr[idx];
end;

constructor TWeatherItem.Create;
begin
  FPressure := 0;
  FDate := 0;
  FDescription := '';
  FTemp := 0;
  FTempMax := 0;
  FTempMin := 0;
  FTempFeelsLike := 0;
  FWindDirection := '';
  FWindSpeed := 0;
  FWindDegrees := 0;
  FWindGust := 0;
  FVisibility := 0;
  FHumidity := 0;
  FSunrise := 0;
  FSunset := 0;
  FWeatherIcon := TMemoryStream.Create;
end;

destructor TWeatherItem.Destroy;
begin
  FWeatherIcon.Free;
  inherited Destroy;
end;

{ TWeatherList }

constructor TWeatherList.Create;
begin
  inherited Create;
  Duplicates := dupIgnore;
  Sorted := True;
end;

function TWeatherList.IndexOfDate(aDate: TDatetime): Integer;
var
  i: Integer;
begin
  Result := -1;
  if Count = 0 then //if no forecast
    exit;
  if (Trunc(aDate) < Trunc(Keys[0])) or (Trunc(aDate) > Trunc(Keys[Count - 1])) then //if the date is out of the forecast
    exit;
  if aDate < Keys[0] then
  begin
    Result := 0;
    exit;
  end;
  for i := 0 to Count - 2 do
  begin
    if aDate < IncSecond(Keys[i], SecondsBetween(Keys[i], Keys[i + 1]) div 2) then
    begin
      Result := i;
      break;
    end;
  end;
  if Result < 0 then
    Result := Count - 1;
end;

{ TWeather }

constructor TWeather.Create;
begin
  FAPIKey := '';
  FLastError := '';
  FLang := wlEn;
  FCity := '';
  FLatitude := 0;
  FLongitude := 0;
  FUnits := wuMetric;
  FCurrentWeather := TWeatherItem.Create;
  FWeatherForecast := TWeatherList.Create;
  if not DirectoryExists(GetAppConfigDir(False)) then
    ForceDirectories(GetAppConfigDir(False));
end;

destructor TWeather.Destroy;
begin
  FCurrentWeather.Free;
  FWeatherForecast.Free;
  inherited Destroy;
end;

procedure TWeather.ClearWeatherInfo;
begin
  FCurrentWeather.Pressure := 0;
  FCurrentWeather.Date := 0;
  FCurrentWeather.Description := '';
  FCurrentWeather.Temp := 0;
  FCurrentWeather.TempMax := 0;
  FCurrentWeather.TempMin := 0;
  FCurrentWeather.TempFeelsLike := 0;
  FCurrentWeather.WindDirection := '';
  FCurrentWeather.WindSpeed := 0;
  FCurrentWeather.WindDegrees := 0;
  FCurrentWeather.WindGust := 0;
  FCurrentWeather.Visibility := 0;
  FCurrentWeather.Humidity := 0;
  FCurrentWeather.Sunrise := 0;
  FCurrentWeather.Sunset := 0;
  FCurrentWeather.WeatherIcon.Clear;
  FWeatherForecast.Clear;
end;

function TWeather.GetLangCode: String;
begin
  case FLang of
    wlAf: Result := 'af';
    wlAl: Result := 'al';
    wlAr: Result := 'ar';
    wlAz: Result := 'az';
    wlBg: Result := 'bg';
    wlCa: Result := 'ca';
    wlCz: Result := 'cz';
    wlDa: Result := 'da';
    wlDe: Result := 'de';
    wlEl: Result := 'el';
    wlEn: Result := 'en';
    wlEs: Result := 'es';
    wlEu: Result := 'eu';
    wlFa: Result := 'fa';
    wlFi: Result := 'fi';
    wlFr: Result := 'fr';
    wlGl: Result := 'gl';
    wlHe: Result := 'he';
    wlHi: Result := 'hi';
    wlHr: Result := 'hr';
    wlHu: Result := 'hu';
    wlId: Result := 'id';
    wlIt: Result := 'it';
    wlJa: Result := 'ja';
    wlKr: Result := 'kr';
    wlLa: Result := 'la';
    wlLt: Result := 'lt';
    wlMk: Result := 'mk';
    wlNo: Result := 'no';
    wlNl: Result := 'nl';
    wlPl: Result := 'pl';
    wlPt: Result := 'pt';
    wlPt_br: Result := 'pt_br';
    wlRo: Result := 'ro';
    wlRu: Result := 'ru';
    wlSv: Result := 'sv';
    wlSk: Result := 'sk';
    wlSl: Result := 'sl';
    wlSr: Result := 'sr';
    wlTh: Result := 'th';
    wlTr: Result := 'tr';
    wlUk: Result := 'uk';
    wlVi: Result := 'vi';
    wlZh_cn: Result := 'zh_cn';
    wlZh_tw: Result := 'zh_tw';
    wlZu: Result := 'zu';
    else
      Result := 'en';
  end;
end;

function TWeather.GetUnitsCode: String;
begin
  case FUnits of
    wuMetric: Result := 'metric';
    wuStandard: Result := 'standard';
    wuImperial: Result := 'imperial';
    else
      Result := 'metric';
  end;
end;

procedure TWeather.Parse(ajo: TJsonObject; var aPogodaItem: TWeatherItem);
var
  i: Integer;
  ja: TJsonArray;
  s: String;
  ss: TStringStream;
begin
  if ajo.FindPath('dt') <> nil then
    aPogodaItem.Date := UnixToDateTime(ajo.GetPath('dt').AsInt64, False);
  if (ajo.FindPath('weather') <> nil) and (ajo.GetPath('weather').Count > 0) then
  begin
    ja := TJsonArray(ajo.GetPath('weather'));
    for i := 0 to ja.Count - 1 do
    begin
      if ja.Items[i].FindPath('description') <> nil then
        aPogodaItem.Description := ja.Items[i].GetPath('description').AsString;
      if ja.Items[i].FindPath('icon') <> nil then
      begin
        //if icon not exists
        if not FileExists(GetAppConfigDir(False) + ja.Items[i].FindPath('icon').AsString + '.png') then
        begin
          //get icon
          s := TFPHttpClient.SimpleGet('http://openweathermap.org/img/wn/' + ja.Items[i].FindPath('icon').AsString + '@2x.png');
          ss := TStringStream.Create(s);
          //an save to disk
          ss.SaveToFile(GetAppConfigDir(False) + ja.Items[i].FindPath('icon').AsString + '.png');
          ss.Free;
        end;
        //load icon
        aPogodaItem.WeatherIcon.Clear;
        aPogodaItem.WeatherIcon.LoadFromFile(GetAppConfigDir(False) + ja.Items[i].FindPath('icon').AsString + '.png');
        aPogodaItem.WeatherIcon.Position := 0;
      end;
    end;
  end;
  if ajo.FindPath('main.temp') <> nil then
    aPogodaItem.Temp := ajo.GetPath('main.temp').AsFloat;
  if ajo.FindPath('main.feels_like') <> nil then
    aPogodaItem.TempFeelsLike := ajo.GetPath('main.feels_like').AsFloat;
  if ajo.FindPath('main.temp_min') <> nil then
    aPogodaItem.TempMin := ajo.GetPath('main.temp_min').AsFloat;
  if ajo.FindPath('main.temp_max') <> nil then
    aPogodaItem.TempMax := ajo.GetPath('main.temp_max').AsFloat;
  if ajo.FindPath('main.humidity') <> nil then
    aPogodaItem.Humidity := ajo.GetPath('main.humidity').AsFloat;
  if ajo.FindPath('main.pressure') <> nil then
    aPogodaItem.Pressure := ajo.GetPath('main.pressure').AsFloat;
  if ajo.FindPath('visibility') <> nil then
    aPogodaItem.Visibility := ajo.GetPath('visibility').AsFloat;
  if ajo.FindPath('wind.speed') <> nil then
    aPogodaItem.WindSpeed := ajo.GetPath('wind.speed').AsFloat;
  if ajo.FindPath('wind.deg') <> nil then
    aPogodaItem.WindDegrees := ajo.GetPath('wind.deg').AsFloat;
  if ajo.FindPath('wind.gust') <> nil then
    aPogodaItem.WindGust := ajo.GetPath('wind.gust').AsFloat;
  if ajo.FindPath('sys.sunrise') <> nil then
    aPogodaItem.Sunrise := UnixToDateTime(ajo.GetPath('sys.sunrise').AsInt64, False);
  if ajo.FindPath('sys.sunset') <> nil then
    aPogodaItem.Sunset := UnixToDateTime(ajo.GetPath('sys.sunset').AsInt64, False);
end;

procedure TWeather.SetLang(AValue: TWeatherLang);
begin
  if FLang <> AValue then
  begin
    ClearWeatherInfo;
    FLang := AValue;
  end;
end;

procedure TWeather.SetLangCode(AValue: String);
begin
  if GetLangCode = AValue then
    exit;
  case AValue of
    'af': FLang := wlAf;
    'al': FLang := wlAl;
    'ar': FLang := wlAr;
    'az': FLang := wlAz;
    'bg': FLang := wlBg;
    'ca': FLang := wlCa;
    'cz': FLang := wlCz;
    'da': FLang := wlDa;
    'de': FLang := wlDe;
    'el': FLang := wlEl;
    'en': FLang := wlEn;
    'es': FLang := wlEs;
    'eu': FLang := wlEu;
    'fa': FLang := wlFa;
    'fi': FLang := wlFi;
    'fr': FLang := wlFr;
    'gl': FLang := wlGl;
    'he': FLang := wlHe;
    'hi': FLang := wlHi;
    'hr': FLang := wlHr;
    'hu': FLang := wlHu;
    'id': FLang := wlId;
    'it': FLang := wlIt;
    'ja': FLang := wlJa;
    'kr': FLang := wlKr;
    'la': FLang := wlLa;
    'lt': FLang := wlLt;
    'mk': FLang := wlMk;
    'no': FLang := wlNo;
    'nl': FLang := wlNl;
    'pl': FLang := wlPl;
    'pt': FLang := wlPt;
    'pt_br': FLang := wlPt_br;
    'ro': FLang := wlRo;
    'ru': FLang := wlRu;
    'sv': FLang := wlSv;
    'sk': FLang := wlSk;
    'sl': FLang := wlSl;
    'sr': FLang := wlSr;
    'th': FLang := wlTh;
    'tr': FLang := wlTr;
    'uk': FLang := wlUk;
    'vi': FLang := wlVi;
    'zh_cn': FLang := wlZh_cn;
    'zh_tw': FLang := wlZh_tw;
    'zu': FLang := wlZu;
    else
      FLang := wlEn;
  end;
end;

procedure TWeather.SetUnits(AValue: TWeatherUnits);
begin
  if FUnits <> AValue then
  begin
    ClearWeatherInfo;
    FUnits := AValue;
  end;
end;

procedure TWeather.SetCity(AValue: String);
begin
  if FCity <> AValue then
  begin
    ClearWeatherInfo;
    FCity := AValue;
  end;
end;

function TWeather.Get(aCity: String; aCountryCode: String; aStateCode: String): Boolean;
var
  json, cc, sc: String;
  ja: TJsonArray;
  lat, lon: Double;
  hc: TFpHttpClient;
begin
  Result := False;
  FLastError := '';
  if FAPIKey = '' then
  begin
    FLastError := 'Enter APIKey';
    exit;
  end;
  if aCity = '' then
  begin
    FLastError := 'Enter city name';
    exit;
  end;
  //country code
  cc := '';
  if aCountryCode <> '' then
    cc := ',' + aCountryCode;
  //state code
  sc := '';
  if (aStateCode <> '') or (SameText(aCountryCode, 'US') or SameText(aCountryCode, 'USA')) then
    sc := aStateCode;
  hc := TFPHttpClient.Create(nil);
  try
    json := hc.Get(Format('http://api.openweathermap.org/geo/1.0/direct?q=%s%s%s&limit=1&appid=%s', [HTTPEncode(aCity), cc, sc, HTTPEncode(FAPIKey)]));
  except
    on E: Exception do
    begin
      if hc.ResponseStatusCode = 401 then
        FLastError := 'Invalid APIKey'
      else
        FLastError := hc.ResponseStatusText;
      hc.Free;
      exit;
    end;
  end;
  hc.Free;
  ja := TJsonArray(GetJSON(json));
  Result := ja.Count > 0;
  if Result and (ja.Items[0].FindPath('lat') <> nil) and (ja.Items[0].FindPath('lon') <> nil) then
  begin
    lat := ja.Items[0].FindPath('lat').AsFloat;
    lon := ja.Items[0].FindPath('lon').AsFloat;
  end
  else
    FLastError := 'Invalid city name';
  ja.Free;
  if Result then
    Result := Get(lat, lon);
end;

function TWeather.Get(aLat, aLon: Double): Boolean;
var
  json: String;
  jo, jod: TJsonObject;
  ja: TJsonArray;
  wi: TWeatherItem;
  i: Integer;
  hc: TFpHttpClient;
  fs: TFormatSettings;
begin
  Result := False;
  FLastError := '';
  if FAPIKey = '' then
  begin
    FLastError := 'Enter APIKey';
    exit;
  end;
  if (aLat = 0) and (aLon = 0) then
  begin
    FLastError := 'Invalid coordinates';
    exit;
  end;
  fs.DecimalSeparator := '.';
  hc := TFPHttpClient.Create(nil);
  try
    json := hc.Get(Format('https://api.openweathermap.org/data/2.5/weather?lat=%.7f&lon=%.7f&appid=%s&units=%s&lang=%s',
      [aLat, aLon, HTTPEncode(FAPIKey), GetUnitsCode, GetLangCode], fs));
  except
    on E: Exception do
    begin
      if hc.ResponseStatusCode = 401 then
        FLastError := 'Invalid APIKey'
      else
        FLastError := hc.ResponseStatusText;
      hc.Free;
      exit;
    end;
  end;
  hc.Free;
  jo := TJsonObject(GetJSON(json));
  Result := jo.FindPath('name') <> nil;
  if Result then
  begin                         
    SetCity(jo.GetPath('name').AsString);
    if jo.FindPath('coord.lat') <> nil then
      FLatitude := jo.FindPath('coord.lat').AsFloat;
    if jo.FindPath('coord.lon') <> nil then
      FLongitude := jo.FindPath('coord.lon').AsFloat;
    FCurrentWeather.Date := Now;
  end;
  Parse(jo, FCurrentWeather);
  jo.Free;
  if not Result then
    exit;
  if (FWeatherForecast.Count = 0) or (Trunc(FWeatherForecast.Keys[0]) < Date) then //if no weather forecast or the weather forecast starts from yesterday or earlier
  begin
    FWeatherForecast.Clear;
    hc := TFPHttpClient.Create(nil);
    try
      json := hc.SimpleGet(Format('https://api.openweathermap.org/data/2.5/forecast?lat=%.7f&lon=%.7f&appid=%s&units=%s&lang=%s',
        [aLat, aLon, HTTPEncode(FAPIKey), GetUnitsCode, GetLangCode], fs));
    except
      on E: Exception do
      begin
        if hc.ResponseStatusCode = 401 then
          FLastError := 'Invalid APIKey'
        else
          FLastError := hc.ResponseStatusText;
        hc.Free;
        exit;
      end;
    end;
    hc.Free;
    jo := TJsonObject(GetJSON(json));
    if jo.FindPath('list') <> nil then
    begin
      ja := TJsonArray(jo.GetPath('list'));
      for i := 0 to ja.Count - 1 do
      begin
        wi := TWeatherItem.Create;
        jod := TJsonObject(ja.Items[i]);
        Parse(jod, wi);
        FWeatherForecast.Add(wi.Date, wi);
      end;
    end;
    jo.Free;
  end;
end;

end.

