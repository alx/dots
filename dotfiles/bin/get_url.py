#!/usr/bin/env python3

import marionette_driver

m = marionette_driver.marionette.Marionette()

m.start_session()
print(m.get_url())
m.delete_session()
