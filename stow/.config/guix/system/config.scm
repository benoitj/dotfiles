;; -*- mode: guix-scheme -*-

(use-modules
 (gnu)
 (gnu packages security-token)
 (gnu packages wm)
 (gnu packages xorg)
 (gnu packages audio)
 (gnu packages pulseaudio)
 (gnu packages certs)
 (gnu packages suckless)
 ((gnu packages linux)
  #:select
  (light))
 (nongnu packages linux)
 (nongnu system linux-initrd)
 ((gnu packages security-token)
  #:select
  (libu2f-host)))


(use-service-modules 
 desktop 
 networking 
 ssh 
 xorg 
 ;; required for pcscd-service
 security-token)

(define dmenu)


(define %backlight-udev-rule
  (udev-rule
   "90-backlight.rules"
   (string-append "ACTION==\"add\", SUBSYSTEM==\"backlight\", "
                  "RUN+=\"/run/current-system/profile/bin/chgrp video /sys/class/backlight/%k/brightness\""
                  "\n"
                  "ACTION==\"add\", SUBSYSTEM==\"backlight\", "
                  "RUN+=\"/run/current-system/profile/bin/chmod g+w /sys/class/backlight/%k/brightness\"")))

(define %my-desktop-services
  (cons*
   (service pcscd-service-type)
   (service mate-desktop-service-type)
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
                                               (udev-configuration-rules config))))))))


(operating-system
 (locale "en_CA.utf8")
 (timezone "America/New_York")
 (keyboard-layout
  (keyboard-layout "us"))
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
     '("wheel" "netdev" "audio" "video" "input" "plugdev")))
   %base-user-accounts))
 (packages
  (append
   (list
    xf86-input-libinput
    pulseaudio
    nss-certs)
   %base-packages))
 (services ;;(cons*
  %my-desktop-services)
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
