;; -*- mode: guix-scheme -*-

(use-modules
 (gnu)
 (gnu packages security-token)
 (gnu packages vim)
 (gnu packages package-management)
 (gnu packages wm)
 (gnu packages xorg)
 (gnu packages audio)
 (gnu packages pulseaudio)
 (gnu packages certs)
 (gnu packages linux)
 (gnu services desktop)
 (gnu services xorg)
 (srfi srfi-1)
 (nongnu packages linux)
 (nongnu system linux-initrd))

(use-service-modules 
 networking 
 ssh 
 security-token)

;; Allow members of the "video" group to change the screen brightness.
(define %backlight-udev-rule
  (udev-rule
   "90-backlight.rules"
   (string-append "ACTION==\"add\", SUBSYSTEM==\"backlight\", "
                  "RUN+=\"/run/current-system/profile/bin/chgrp video /sys/class/backlight/%k/brightness\""
                  "\n"
                  "ACTION==\"add\", SUBSYSTEM==\"backlight\", "
                  "RUN+=\"/run/current-system/profile/bin/chmod g+w /sys/class/backlight/%k/brightness\"")))

(define %xorg-libinput-config
  "Section \"InputClass\"
  Identifier \"Touchpads\"
  Driver \"libinput\"
  MatchDevicePath \"/dev/input/event*\"
  MatchIsTouchpad \"on\"

  Option \"Tapping\" \"on\"
  Option \"TappingDrag\" \"on\"
  Option \"DisableWhileTyping\" \"on\"
  Option \"MiddleEmulation\" \"on\"
  Option \"ScrollMethod\" \"twofinger\"
EndSection
Section \"InputClass\"
  Identifier \"Keyboards\"
  Driver \"libinput\"
  MatchDevicePath \"/dev/input/event*\"
  MatchIsKeyboard \"on\"
EndSection
")


(define %my-desktop-services
  (modify-services %desktop-services
                   (elogind-service-type config =>
                                         (elogind-configuration
                                          (inherit config)
                                          (handle-lid-switch-external-power 'suspend)))
                   (udev-service-type config =>
                                      (udev-configuration
                                       (inherit config)
                                       (rules
                                        (cons %backlight-udev-rule
                                              (udev-configuration-rules config)))))))
(operating-system
 (locale "en_CA.utf8")
 (timezone "America/New_York")
 (keyboard-layout (keyboard-layout "us" #:model "thinkpad"))
 (host-name "milhouse")
 (kernel linux)
 (initrd microcode-initrd)
 (firmware
  (list linux-firmware))
 (groups
  (append
   (list
    (user-group
     (name "plugdev")))
   %base-groups))
 (users
  (cons*
   (user-account
    (name "benoit")
    (comment "Benoit")
    (group "users")
    (home-directory "/home/benoit")
    (supplementary-groups
     '("wheel"
       "netdev"
       "lp" ;; bluetooth
       "audio"
       "video"
       "input")))
   %base-user-accounts))
 (packages
  (append
   (list
    vim
    stow
    xf86-input-libinput
    bluez
    bluez-alsa
    pulseaudio
    nss-certs)
   %base-packages))
 (services
  (cons*
   (service pcscd-service-type)
   (service slim-service-type
            (slim-configuration
             (default-user "benoit")
             (xorg-configuration
              (xorg-configuration
               (keyboard-layout keyboard-layout)
               (extra-config (list %xorg-libinput-config))))))
   (bluetooth-service #:auto-enable? #t)
   (remove  
    (lambda
        (service)
      (eq?
       (service-kind service)
       gdm-service-type))
    %my-desktop-services)))
 (bootloader
  (bootloader-configuration
   (bootloader grub-efi-bootloader)
   (target "/boot/efi")
   (keyboard-layout keyboard-layout)))
 (mapped-devices
  (list
   (mapped-device
    (source
     (uuid "6cbeeb9b-2a7c-4347-a93a-5a33c355e5e2"))
    (target "cryptroot")
    (type luks-device-mapping))
   (mapped-device
    (source
     (uuid "e12ddf36-fc44-41cc-a081-3be57aeb15f2"))
    (target "home")
    (type luks-device-mapping))))
 (file-systems
  (cons*
   (file-system
    (mount-point "/boot/efi")
    (device
     (uuid "99B8-A614" 'fat32))
    (type "vfat"))
   (file-system
    (mount-point "/")
    (device "/dev/mapper/cryptroot")
    (type "ext4")
    (dependencies mapped-devices))
   (file-system
    (mount-point "/home")
    (device "/dev/mapper/home")
    (type "ext4")
    (dependencies mapped-devices))
   %base-file-systems)))
