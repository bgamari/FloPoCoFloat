FROM amd64/debian:11
RUN apt-get update
RUN apt-get install -y zlib1g  wget curl
RUN apt-get install -y build-essential curl libffi-dev libffi7 libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5 pkg-config git
RUN apt-get install -y bash-completion vim

ENV BOOTSTRAP_HASKELL_NONINTERACTIVE=1
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

RUN apt-get install -y libmpfr6 libmpfr-dev
RUN ~/.ghcup/bin/ghcup install ghc 9.8.2
RUN ~/.ghcup/bin/ghcup set ghc 9.8.2

COPY . FloPoCoFloat
RUN cd FloPoCoFloat && ~/.ghcup/bin/cabal build -w $HOME/.ghcup/bin/ghc-9.8.2

