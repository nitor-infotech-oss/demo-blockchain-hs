FROM haskell:8.4.4
RUN apt-get install git -y
RUN git clone https://gitlab.com/saurabhkukade/demo-blockchain-hs.git
WORKDIR demo-blockchain-hs
RUN cabal update
CMD ["ghc"]

