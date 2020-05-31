# LOGGERS

Logs messages it receives over http into a discord channel.

## Configuration:
Settings are either taken from an environment variable or from a plain text file.

|variable|environment variable|filename|
|-|-|-|
|Discord oauth token|DISCORD\_TOKEN|discord\_token|
|Discord channel id|DISCORD\_CHANNEL\_ID|discord\_channel\_id|

## Usage
Create a `POST` request to `localhost:8075/error/<app name>`.
The raw body of the request will be output to discord.
