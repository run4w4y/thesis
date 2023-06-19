FROM haskell:9.2.7

COPY . /app
WORKDIR /app

RUN stack setup && stack build --only-dependencies
RUN stack build

CMD stack exec notes-app
