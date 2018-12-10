# Delphi WkHtmlToX Library

[Delphi](http://www.embarcadero.com/products/delphi) implementation of [WkHtmlToX](https://wkhtmltopdf.org/).

## What is it?

Delphi `WkHtmlToX Library` is an open source ([Apache license](https://www.apache.org/licenses/LICENSE-2.0)) wrapper around WkHtmlToX. You can find a basic header traslation from the C API (WkHtmlToX.Bindings.pas) and few classes that simplify its usage (WkHtmlToX.Core.pas).
Inside the *sample* folder you can find 4 demos:

* *ApiDemo*: a simple example that convert a HTML using only the raw API 
* *WrapperDemo*: same as before but with `IWkHtmlToPdf` class
* *ThreadedDemo*: a thread-safe sample
* *StreamDemo*: another example using HTML from memory instead of file

WkHtmlToX has several configuration options, you can find a comprehensive list on the [original documentation page](https://wkhtmltopdf.org/libwkhtmltox/pagesettings.html).


## Usage

*Source code:*

```
#!delphi
var
  ObjectSettings: IWkObjectSettings;
  GlobalSettings: IWkGlobalSettings;
  Converter: IWkConverter;
begin
  GlobalSettings := WkHtmlToPdf.CreateGlobalSettings;
  // We want the result to be storred in the file called test.pdf 
  GlobalSettings['out'] := 'test.pdf';

  ObjectSettings := WkHtmlToPdf.CreateObjectSettings;
  // We want to convert the url 'https://wkhtmltopdf.org/'
  ObjectSettings['page'] := 'https://wkhtmltopdf.org/';

  Converter := WkHtmlToPdf.CreateConverter(GlobalSettings);
  // Add the settings object to the list of pages to convert. 
  // Objects are converted in the order in which they are added
  Converter.AddObject(ObjectSettings);

  Converter.Convert;
end;
```

You can find the GlobalSettings and ObjectSettings reference guide here:
[https://wkhtmltopdf.org/libwkhtmltox/pagesettings.html](https://wkhtmltopdf.org/libwkhtmltox/pagesettings.html)

## Issues

- CharSet: in-memory Html conversion doesn't support unicode. You can use metatag as a work-around (see StreamDemo) or use HTML Entities.
- Thread-safety: the tool can convert only a document at once. You can find a work-around in the ThreadDemo.

## Prerequisite

WkHtmlToX library must be in your system path.
