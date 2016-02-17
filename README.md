# [DEPRECATED] No longer being actively developed

[![Build Status](https://travis-ci.org/fhirbase/fhirterm.svg?branch=master)](https://travis-ci.org/fhirbase/fhirterm)

# fhirterm

Standalone implementation of
[FHIR Terminology Service](http://www.hl7.org/implement/standards/FHIR-Develop/terminology-service.html).

Powered by [Health Samurai](http://healthsamurai.github.io/)

Sponsored by:

![choice-hs.com](http://choice-hs.com/Images/Shared/Choice-HSLogo.png)

## Notices

Command to create DB backup:

```
$ pg_dump -h localhost -U fhirterm -T netrika_lab_services --exclude-table-data=custom_naming_systems -x --no-owner --no-acl -W -d fhirterm -F c -Z 9 -f fhirterm.backup
```

## License

Copyright © 2014 Health Samurai Team

Distributed under the MIT License.
