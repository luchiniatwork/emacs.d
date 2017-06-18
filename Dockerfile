# Why?

# The motivation is to have a simple container that could be instantiated anywhere
# (hopefully in a machine more powerful than my own) and then be used remotely via
# emacsclient

# We would build this image with something like:
# $ docker build . --tag emacs

# And then run the container like this:
# $ docker run -it -p 4545:4545 -v /tmp/server:/root/.emacs.d/server/ --rm emacs emacs --debug-init

# However, emacs (smartly) despises the mount to the host above as t considers it unsafe
# (persmissions are too giving).

FROM openjdk:8-jre

# Install lein
ADD https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein /bin
RUN chmod +x /bin/lein

# Install dependencies
RUN apt-get update && \
    apt-get install -y curl git emacs24

WORKDIR /root

# Copy the scripts we'll need
COPY init.el .emacs.d/
COPY customizations/* .emacs.d/customizations/

# Run the initialization script in order to install everything as part of the image
# Additionally, pipe warnings and errors to true because they will inevitably happen
RUN HOME=/root emacs --script .emacs.d/init.el 2>&1 || true

EXPOSE 4545

CMD emacs
