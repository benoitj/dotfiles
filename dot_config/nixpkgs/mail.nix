{ pkgs, ... }:

{
  home.packages = with pkgs; [
    notmuch
    isync
  ];

  services.imapnotify.enable = true;

  accounts.email.accounts.benoit_at_benoitj_ca = {
    primary = true;
    address = "benoit@benoitj.ca";
    imapnotify = {
      enable = true;
      boxes = [ "Inbox" ];
      onNotify = "mail-sync.sh && notify-send 'New email'";
    };
    imap = {
      host = "mail.benoitj.ca";
      port = 993;
      tls = {
        enable = true;
      };
    };
    userName = "benoit@benoitj.ca";
    passwordCommand = [ "passp" "mailbox.org" ];
  };
}
