{ pkgs, ... }:

{
  home.packages = with pkgs; [
    notmuch
    isync
  ];

  services.imapnotify.enable = true;
  programs.msmtp.enable = true;

  accounts.email.accounts.benoit_at_benoitj_ca = {
    primary = true;

    address = "benoit@benoitj.ca";
    userName = "benoit@benoitj.ca";
    passwordCommand = [ "passp" "mailbox.org" ];

    imapnotify = {
      enable = true;
      boxes = [ "Inbox" ];
      onNotify = "mail-sync.sh && notify-send 'New email'";
    };

    imap = {
      host = "mail.benoitj.ca";
      port = 993;
      tls.enable = true;
    };

    smtp = {
      host = "mail.benoitj.ca";
      port = 465;
      tls.enable = true;
    };

    msmtp = {
      enable = true;
    };
  };
}
