rm -rf rpmbuild ; mkdir rpmbuild ; mkdir rpmbuild/SOURCES ; mkdir rpmbuild/SPECS ; cp redhat/*.spec rpmbuild/SPECS/ ; cp quagga*.tar.gz rpmbuild/SOURCES/ ; cp quagga.spec rpmbuild/SPECS/ ; rpmbuild --define "_topdir `pwd`/rpmbuild" -ba rpmbuild/SPECS/quagga.spec ; ls ./rpmbuild/RPMS/x86_6 ; tar cfzv quagga-rpm.tgz -C rpmbuild/RPMS/x86_64 . ; mv quagga-rpm.tgz .. 