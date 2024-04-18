FROM ubuntu:20.04

## BEGIN: RUNS AS ROOT

# Create a user

ARG USERNAME=cos320
ARG USER_UID=1000
ARG USER_GID=$USER_UID

RUN groupadd --gid $USER_GID $USERNAME \
    && useradd --uid $USER_UID --gid $USER_GID -m $USERNAME --shell /bin/zsh \
    #
    # [Optional] Add sudo support. Omit if you don't need to install software after connecting.
    && apt-get update -y \
    && apt-get install -y sudo \
    && echo $USERNAME ALL=\(root\) NOPASSWD:ALL > /etc/sudoers.d/$USERNAME \
    && chmod 0440 /etc/sudoers.d/$USERNAME

## Hack needs root permissions

# See hack.sh
COPY hack.sh /tmp/hack.sh
RUN chmod +x /tmp/hack.sh
RUN /tmp/hack.sh

RUN apt-get install -y build-essential
RUN apt-get install -y m4
RUN apt-get install -y opam
RUN apt-get install -y clang
RUN apt-get install -y time
RUN apt-get install -y zip
RUN apt-get install -y zsh

## Set up user environmnent
COPY .zshrc /home/$USERNAME/


## Run in usermode

# [Optional] Set the default user. Omit if you want to keep the default as root.
USER $USERNAME

RUN mkdir -p /home/$USERNAME/.local/state/
RUN touch /home/$USERNAME/.local/state/utop-history

# Configure opam/ocaml
RUN opam init -y --disable-sandboxing --compiler=4.13.1
RUN opam switch 4.13.1
RUN opam install -y dune
RUN opam install -y num
RUN opam install -y menhir
RUN opam install -y utop
RUN opam install -y ocamlformat
RUN opam install -y ocaml-lsp-server
RUN eval `opam config env`

