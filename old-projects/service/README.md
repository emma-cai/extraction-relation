# Ermine service

This is a server that has a configured set of Ermine pipelines that it can run.

Pipelines are loaded from [application.conf](https://github.com/allenai/extraction/tree/master/service/src/main/resources/application.conf)
at startup.

The service handles [requests](https://github.com/allenai/extraction/blob/master/api/src/main/scala/org/allenai/extraction/api/JsonProtocol.scala#L10)
on `/pipeline/{name}`. When getting a request for the subpath "name", the service will run the pipeline
named "name" in the config file, using the provided named inputs.

There is currently no way to configure default (unnamed) inputs in the config file.

# Pre-deploy Installtion on EC2

Create the directory `/local/deploy/ermine-service`, and make sure it's writable by the default user.

# Deploying

Once your server is set up, execute `sbt 'project service' 'deploy prod'` to deploy!
