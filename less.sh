#!/bin/sh

APP=apps/web/priv/static/less
CSS=apps/web/priv/static/css

/usr/bin/node apps/web/priv/static/less.js/bin/lessc -x $APP/countach.less > $CSS/countach.css

echo $?

