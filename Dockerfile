FROM debian
WORKDIR /workspace/bin/

COPY sources.list /etc/apt/
RUN apt-get update; apt-get install wget -y; apt-get install xz-utils make -y; apt-get build-dep ghc -y
RUN wget https://downloads.haskell.org/~ghc/8.4.4/ghc-8.4.4-x86_64-deb9-linux.tar.xz
RUN tar -xvf ghc-8.4.4-x86_64-deb9-linux.tar.xz
RUN ./ghc-8.4.4/configure
RUN cd ghc-8.4.4
RUN make install -j4

CMD ["python","/Dodge/DodgeReports/app/v1/app.py"]

