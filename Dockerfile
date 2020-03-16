FROM haskell:8.4.4
RUN apt-get install git -y
RUN git clone https://github.com/nitor-infotech-oss/demo-blockchain-hs.git
WORKDIR demo-blockchain-hs
RUN cabal v2-update
RUN cabal v2-build
CMD ["make", "run"]

