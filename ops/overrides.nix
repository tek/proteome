{
dev = {
  chiasma = {
  meta = {
    sha256 = "05y3x5244ibmqkwr0b3lhnl01az4ry58i3fgrkifyibg9br8gxgm";
    ver = "0.11.0.0";
  };
  drv = { mkDerivation, attoparsec, base, bytestring, composition
, containers, exon, extra, first-class-families, lens, lib, parsec
, parsers, path, polysemy, polysemy-conc, polysemy-log
, polysemy-plugin, polysemy-process, polysemy-time, prelate
, prettyprinter, prettyprinter-ansi-terminal, random, text
, transformers, typed-process, uuid
}:
mkDerivation {
  pname = "chiasma";
  version = "0.11.0.0";
  src = /nix/store/lnmnppx3g7dd22fmmxmy8dx172d1f6zj-source;
  libraryHaskellDepends = [
    attoparsec base bytestring composition containers exon extra
    first-class-families lens parsec parsers path polysemy
    polysemy-conc polysemy-log polysemy-plugin polysemy-process
    polysemy-time prelate prettyprinter prettyprinter-ansi-terminal
    random text transformers typed-process uuid
  ];
  homepage = "https://github.com/tek/chiasma#readme";
  description = "A tmux client for Polysemy";
  license = "BSD-2-Clause-Patent";
}
;
}
;
  chiasma-test = {
  meta = {
    sha256 = "15l7hvkpgw2fv4hxkmw4xdja3a30n5bj2g69rc8gppfwpjr0bkhg";
    ver = "0.11.0.0";
  };
  drv = { mkDerivation, base, bytestring, chiasma, chronos, exon, hedgehog
, lens, lib, path, path-io, polysemy, polysemy-chronos
, polysemy-conc, polysemy-log, polysemy-plugin, polysemy-process
, polysemy-test, polysemy-time, prelate, tasty, tasty-hedgehog
, text, typed-process
}:
mkDerivation {
  pname = "chiasma-test";
  version = "0.11.0.0";
  src = /nix/store/sha110izksz2wcqmw85vyh3nhf9hir2h-source;
  libraryHaskellDepends = [
    base bytestring chiasma chronos exon hedgehog path path-io polysemy
    polysemy-chronos polysemy-conc polysemy-log polysemy-plugin
    polysemy-process polysemy-test polysemy-time prelate text
    typed-process
  ];
  testHaskellDepends = [
    base chiasma hedgehog lens path-io polysemy polysemy-chronos
    polysemy-plugin polysemy-test prelate tasty tasty-hedgehog
  ];
  homepage = "https://github.com/tek/chiasma#readme";
  description = "Testing tools for chiasma";
  license = "BSD-2-Clause-Patent";
}
;
}
;
  streamly-process = {
  meta = {
    sha256 = "19czzdf68c13vd17587vmq1d58plh99zyj029zy9a3j3s0nqakgh";
    ver = "0.4.0";
  };
  drv = { mkDerivation, base, directory, exceptions, hspec, lib, process
, QuickCheck, streamly, streamly-core, tasty-bench
}:
mkDerivation {
  pname = "streamly-process";
  version = "0.4.0";
  src = /nix/store/s3x9q034qgg8j5wls3iajs40j17cg6w1-source;
  libraryHaskellDepends = [
    base exceptions process streamly streamly-core
  ];
  testHaskellDepends = [
    base directory exceptions hspec QuickCheck streamly-core
  ];
  benchmarkHaskellDepends = [
    base directory streamly-core tasty-bench
  ];
  homepage = "https://streamly.composewell.com";
  description = "Use OS processes as stream transformation functions";
  license = lib.licenses.asl20;
}
;
}
;
};
ghc910 = {
  chiasma = {
  meta = {
    sha256 = "05y3x5244ibmqkwr0b3lhnl01az4ry58i3fgrkifyibg9br8gxgm";
    ver = "0.11.0.0";
  };
  drv = { mkDerivation, attoparsec, base, bytestring, composition
, containers, exon, extra, first-class-families, lens, lib, parsec
, parsers, path, polysemy, polysemy-conc, polysemy-log
, polysemy-plugin, polysemy-process, polysemy-time, prelate
, prettyprinter, prettyprinter-ansi-terminal, random, text
, transformers, typed-process, uuid
}:
mkDerivation {
  pname = "chiasma";
  version = "0.11.0.0";
  src = /nix/store/lnmnppx3g7dd22fmmxmy8dx172d1f6zj-source;
  libraryHaskellDepends = [
    attoparsec base bytestring composition containers exon extra
    first-class-families lens parsec parsers path polysemy
    polysemy-conc polysemy-log polysemy-plugin polysemy-process
    polysemy-time prelate prettyprinter prettyprinter-ansi-terminal
    random text transformers typed-process uuid
  ];
  homepage = "https://github.com/tek/chiasma#readme";
  description = "A tmux client for Polysemy";
  license = "BSD-2-Clause-Patent";
}
;
}
;
  chiasma-test = {
  meta = {
    sha256 = "15l7hvkpgw2fv4hxkmw4xdja3a30n5bj2g69rc8gppfwpjr0bkhg";
    ver = "0.11.0.0";
  };
  drv = { mkDerivation, base, bytestring, chiasma, chronos, exon, hedgehog
, lens, lib, path, path-io, polysemy, polysemy-chronos
, polysemy-conc, polysemy-log, polysemy-plugin, polysemy-process
, polysemy-test, polysemy-time, prelate, tasty, tasty-hedgehog
, text, typed-process
}:
mkDerivation {
  pname = "chiasma-test";
  version = "0.11.0.0";
  src = /nix/store/sha110izksz2wcqmw85vyh3nhf9hir2h-source;
  libraryHaskellDepends = [
    base bytestring chiasma chronos exon hedgehog path path-io polysemy
    polysemy-chronos polysemy-conc polysemy-log polysemy-plugin
    polysemy-process polysemy-test polysemy-time prelate text
    typed-process
  ];
  testHaskellDepends = [
    base chiasma hedgehog lens path-io polysemy polysemy-chronos
    polysemy-plugin polysemy-test prelate tasty tasty-hedgehog
  ];
  homepage = "https://github.com/tek/chiasma#readme";
  description = "Testing tools for chiasma";
  license = "BSD-2-Clause-Patent";
}
;
}
;
  streamly-process = {
  meta = {
    sha256 = "19czzdf68c13vd17587vmq1d58plh99zyj029zy9a3j3s0nqakgh";
    ver = "0.4.0";
  };
  drv = { mkDerivation, base, directory, exceptions, hspec, lib, process
, QuickCheck, streamly, streamly-core, tasty-bench
}:
mkDerivation {
  pname = "streamly-process";
  version = "0.4.0";
  src = /nix/store/s3x9q034qgg8j5wls3iajs40j17cg6w1-source;
  libraryHaskellDepends = [
    base exceptions process streamly streamly-core
  ];
  testHaskellDepends = [
    base directory exceptions hspec QuickCheck streamly-core
  ];
  benchmarkHaskellDepends = [
    base directory streamly-core tasty-bench
  ];
  homepage = "https://streamly.composewell.com";
  description = "Use OS processes as stream transformation functions";
  license = lib.licenses.asl20;
}
;
}
;
};
ghc912 = {
  chiasma = {
  meta = {
    sha256 = "05y3x5244ibmqkwr0b3lhnl01az4ry58i3fgrkifyibg9br8gxgm";
    ver = "0.11.0.0";
  };
  drv = { mkDerivation, attoparsec, base, bytestring, composition
, containers, exon, extra, first-class-families, lens, lib, parsec
, parsers, path, polysemy, polysemy-conc, polysemy-log
, polysemy-plugin, polysemy-process, polysemy-time, prelate
, prettyprinter, prettyprinter-ansi-terminal, random, text
, transformers, typed-process, uuid
}:
mkDerivation {
  pname = "chiasma";
  version = "0.11.0.0";
  src = /nix/store/lnmnppx3g7dd22fmmxmy8dx172d1f6zj-source;
  libraryHaskellDepends = [
    attoparsec base bytestring composition containers exon extra
    first-class-families lens parsec parsers path polysemy
    polysemy-conc polysemy-log polysemy-plugin polysemy-process
    polysemy-time prelate prettyprinter prettyprinter-ansi-terminal
    random text transformers typed-process uuid
  ];
  homepage = "https://github.com/tek/chiasma#readme";
  description = "A tmux client for Polysemy";
  license = "BSD-2-Clause-Patent";
}
;
}
;
  chiasma-test = {
  meta = {
    sha256 = "15l7hvkpgw2fv4hxkmw4xdja3a30n5bj2g69rc8gppfwpjr0bkhg";
    ver = "0.11.0.0";
  };
  drv = { mkDerivation, base, bytestring, chiasma, chronos, exon, hedgehog
, lens, lib, path, path-io, polysemy, polysemy-chronos
, polysemy-conc, polysemy-log, polysemy-plugin, polysemy-process
, polysemy-test, polysemy-time, prelate, tasty, tasty-hedgehog
, text, typed-process
}:
mkDerivation {
  pname = "chiasma-test";
  version = "0.11.0.0";
  src = /nix/store/sha110izksz2wcqmw85vyh3nhf9hir2h-source;
  libraryHaskellDepends = [
    base bytestring chiasma chronos exon hedgehog path path-io polysemy
    polysemy-chronos polysemy-conc polysemy-log polysemy-plugin
    polysemy-process polysemy-test polysemy-time prelate text
    typed-process
  ];
  testHaskellDepends = [
    base chiasma hedgehog lens path-io polysemy polysemy-chronos
    polysemy-plugin polysemy-test prelate tasty tasty-hedgehog
  ];
  homepage = "https://github.com/tek/chiasma#readme";
  description = "Testing tools for chiasma";
  license = "BSD-2-Clause-Patent";
}
;
}
;
  streamly-process = {
  meta = {
    sha256 = "19czzdf68c13vd17587vmq1d58plh99zyj029zy9a3j3s0nqakgh";
    ver = "0.4.0";
  };
  drv = { mkDerivation, base, directory, exceptions, hspec, lib, process
, QuickCheck, streamly, streamly-core, tasty-bench
}:
mkDerivation {
  pname = "streamly-process";
  version = "0.4.0";
  src = /nix/store/s3x9q034qgg8j5wls3iajs40j17cg6w1-source;
  libraryHaskellDepends = [
    base exceptions process streamly streamly-core
  ];
  testHaskellDepends = [
    base directory exceptions hspec QuickCheck streamly-core
  ];
  benchmarkHaskellDepends = [
    base directory streamly-core tasty-bench
  ];
  homepage = "https://streamly.composewell.com";
  description = "Use OS processes as stream transformation functions";
  license = lib.licenses.asl20;
}
;
}
;
};
ghc98 = {
  chiasma = {
  meta = {
    sha256 = "05y3x5244ibmqkwr0b3lhnl01az4ry58i3fgrkifyibg9br8gxgm";
    ver = "0.11.0.0";
  };
  drv = { mkDerivation, attoparsec, base, bytestring, composition
, containers, exon, extra, first-class-families, lens, lib, parsec
, parsers, path, polysemy, polysemy-conc, polysemy-log
, polysemy-plugin, polysemy-process, polysemy-time, prelate
, prettyprinter, prettyprinter-ansi-terminal, random, text
, transformers, typed-process, uuid
}:
mkDerivation {
  pname = "chiasma";
  version = "0.11.0.0";
  src = /nix/store/lnmnppx3g7dd22fmmxmy8dx172d1f6zj-source;
  libraryHaskellDepends = [
    attoparsec base bytestring composition containers exon extra
    first-class-families lens parsec parsers path polysemy
    polysemy-conc polysemy-log polysemy-plugin polysemy-process
    polysemy-time prelate prettyprinter prettyprinter-ansi-terminal
    random text transformers typed-process uuid
  ];
  homepage = "https://github.com/tek/chiasma#readme";
  description = "A tmux client for Polysemy";
  license = "BSD-2-Clause-Patent";
}
;
}
;
  chiasma-test = {
  meta = {
    sha256 = "15l7hvkpgw2fv4hxkmw4xdja3a30n5bj2g69rc8gppfwpjr0bkhg";
    ver = "0.11.0.0";
  };
  drv = { mkDerivation, base, bytestring, chiasma, chronos, exon, hedgehog
, lens, lib, path, path-io, polysemy, polysemy-chronos
, polysemy-conc, polysemy-log, polysemy-plugin, polysemy-process
, polysemy-test, polysemy-time, prelate, tasty, tasty-hedgehog
, text, typed-process
}:
mkDerivation {
  pname = "chiasma-test";
  version = "0.11.0.0";
  src = /nix/store/sha110izksz2wcqmw85vyh3nhf9hir2h-source;
  libraryHaskellDepends = [
    base bytestring chiasma chronos exon hedgehog path path-io polysemy
    polysemy-chronos polysemy-conc polysemy-log polysemy-plugin
    polysemy-process polysemy-test polysemy-time prelate text
    typed-process
  ];
  testHaskellDepends = [
    base chiasma hedgehog lens path-io polysemy polysemy-chronos
    polysemy-plugin polysemy-test prelate tasty tasty-hedgehog
  ];
  homepage = "https://github.com/tek/chiasma#readme";
  description = "Testing tools for chiasma";
  license = "BSD-2-Clause-Patent";
}
;
}
;
  streamly-process = {
  meta = {
    sha256 = "19czzdf68c13vd17587vmq1d58plh99zyj029zy9a3j3s0nqakgh";
    ver = "0.4.0";
  };
  drv = { mkDerivation, base, directory, exceptions, hspec, lib, process
, QuickCheck, streamly, streamly-core, tasty-bench
}:
mkDerivation {
  pname = "streamly-process";
  version = "0.4.0";
  src = /nix/store/s3x9q034qgg8j5wls3iajs40j17cg6w1-source;
  libraryHaskellDepends = [
    base exceptions process streamly streamly-core
  ];
  testHaskellDepends = [
    base directory exceptions hspec QuickCheck streamly-core
  ];
  benchmarkHaskellDepends = [
    base directory streamly-core tasty-bench
  ];
  homepage = "https://streamly.composewell.com";
  description = "Use OS processes as stream transformation functions";
  license = lib.licenses.asl20;
}
;
}
;
};
hix-build-tools = {
};
hls = {
};
min = {
  chiasma = {
  meta = {
    sha256 = "05y3x5244ibmqkwr0b3lhnl01az4ry58i3fgrkifyibg9br8gxgm";
    ver = "0.11.0.0";
  };
  drv = { mkDerivation, attoparsec, base, bytestring, composition
, containers, exon, extra, first-class-families, lens, lib, parsec
, parsers, path, polysemy, polysemy-conc, polysemy-log
, polysemy-plugin, polysemy-process, polysemy-time, prelate
, prettyprinter, prettyprinter-ansi-terminal, random, text
, transformers, typed-process, uuid
}:
mkDerivation {
  pname = "chiasma";
  version = "0.11.0.0";
  src = /nix/store/lnmnppx3g7dd22fmmxmy8dx172d1f6zj-source;
  libraryHaskellDepends = [
    attoparsec base bytestring composition containers exon extra
    first-class-families lens parsec parsers path polysemy
    polysemy-conc polysemy-log polysemy-plugin polysemy-process
    polysemy-time prelate prettyprinter prettyprinter-ansi-terminal
    random text transformers typed-process uuid
  ];
  homepage = "https://github.com/tek/chiasma#readme";
  description = "A tmux client for Polysemy";
  license = "BSD-2-Clause-Patent";
}
;
}
;
  chiasma-test = {
  meta = {
    sha256 = "15l7hvkpgw2fv4hxkmw4xdja3a30n5bj2g69rc8gppfwpjr0bkhg";
    ver = "0.11.0.0";
  };
  drv = { mkDerivation, base, bytestring, chiasma, chronos, exon, hedgehog
, lens, lib, path, path-io, polysemy, polysemy-chronos
, polysemy-conc, polysemy-log, polysemy-plugin, polysemy-process
, polysemy-test, polysemy-time, prelate, tasty, tasty-hedgehog
, text, typed-process
}:
mkDerivation {
  pname = "chiasma-test";
  version = "0.11.0.0";
  src = /nix/store/sha110izksz2wcqmw85vyh3nhf9hir2h-source;
  libraryHaskellDepends = [
    base bytestring chiasma chronos exon hedgehog path path-io polysemy
    polysemy-chronos polysemy-conc polysemy-log polysemy-plugin
    polysemy-process polysemy-test polysemy-time prelate text
    typed-process
  ];
  testHaskellDepends = [
    base chiasma hedgehog lens path-io polysemy polysemy-chronos
    polysemy-plugin polysemy-test prelate tasty tasty-hedgehog
  ];
  homepage = "https://github.com/tek/chiasma#readme";
  description = "Testing tools for chiasma";
  license = "BSD-2-Clause-Patent";
}
;
}
;
  streamly-process = {
  meta = {
    sha256 = "19czzdf68c13vd17587vmq1d58plh99zyj029zy9a3j3s0nqakgh";
    ver = "0.4.0";
  };
  drv = { mkDerivation, base, directory, exceptions, hspec, lib, process
, QuickCheck, streamly, streamly-core, tasty-bench
}:
mkDerivation {
  pname = "streamly-process";
  version = "0.4.0";
  src = /nix/store/s3x9q034qgg8j5wls3iajs40j17cg6w1-source;
  libraryHaskellDepends = [
    base exceptions process streamly streamly-core
  ];
  testHaskellDepends = [
    base directory exceptions hspec QuickCheck streamly-core
  ];
  benchmarkHaskellDepends = [
    base directory streamly-core tasty-bench
  ];
  homepage = "https://streamly.composewell.com";
  description = "Use OS processes as stream transformation functions";
  license = lib.licenses.asl20;
}
;
}
;
};
profiled = {
  chiasma = {
  meta = {
    sha256 = "05y3x5244ibmqkwr0b3lhnl01az4ry58i3fgrkifyibg9br8gxgm";
    ver = "0.11.0.0";
  };
  drv = { mkDerivation, attoparsec, base, bytestring, composition
, containers, exon, extra, first-class-families, lens, lib, parsec
, parsers, path, polysemy, polysemy-conc, polysemy-log
, polysemy-plugin, polysemy-process, polysemy-time, prelate
, prettyprinter, prettyprinter-ansi-terminal, random, text
, transformers, typed-process, uuid
}:
mkDerivation {
  pname = "chiasma";
  version = "0.11.0.0";
  src = /nix/store/lnmnppx3g7dd22fmmxmy8dx172d1f6zj-source;
  libraryHaskellDepends = [
    attoparsec base bytestring composition containers exon extra
    first-class-families lens parsec parsers path polysemy
    polysemy-conc polysemy-log polysemy-plugin polysemy-process
    polysemy-time prelate prettyprinter prettyprinter-ansi-terminal
    random text transformers typed-process uuid
  ];
  homepage = "https://github.com/tek/chiasma#readme";
  description = "A tmux client for Polysemy";
  license = "BSD-2-Clause-Patent";
}
;
}
;
  chiasma-test = {
  meta = {
    sha256 = "15l7hvkpgw2fv4hxkmw4xdja3a30n5bj2g69rc8gppfwpjr0bkhg";
    ver = "0.11.0.0";
  };
  drv = { mkDerivation, base, bytestring, chiasma, chronos, exon, hedgehog
, lens, lib, path, path-io, polysemy, polysemy-chronos
, polysemy-conc, polysemy-log, polysemy-plugin, polysemy-process
, polysemy-test, polysemy-time, prelate, tasty, tasty-hedgehog
, text, typed-process
}:
mkDerivation {
  pname = "chiasma-test";
  version = "0.11.0.0";
  src = /nix/store/sha110izksz2wcqmw85vyh3nhf9hir2h-source;
  libraryHaskellDepends = [
    base bytestring chiasma chronos exon hedgehog path path-io polysemy
    polysemy-chronos polysemy-conc polysemy-log polysemy-plugin
    polysemy-process polysemy-test polysemy-time prelate text
    typed-process
  ];
  testHaskellDepends = [
    base chiasma hedgehog lens path-io polysemy polysemy-chronos
    polysemy-plugin polysemy-test prelate tasty tasty-hedgehog
  ];
  homepage = "https://github.com/tek/chiasma#readme";
  description = "Testing tools for chiasma";
  license = "BSD-2-Clause-Patent";
}
;
}
;
  streamly-process = {
  meta = {
    sha256 = "19czzdf68c13vd17587vmq1d58plh99zyj029zy9a3j3s0nqakgh";
    ver = "0.4.0";
  };
  drv = { mkDerivation, base, directory, exceptions, hspec, lib, process
, QuickCheck, streamly, streamly-core, tasty-bench
}:
mkDerivation {
  pname = "streamly-process";
  version = "0.4.0";
  src = /nix/store/s3x9q034qgg8j5wls3iajs40j17cg6w1-source;
  libraryHaskellDepends = [
    base exceptions process streamly streamly-core
  ];
  testHaskellDepends = [
    base directory exceptions hspec QuickCheck streamly-core
  ];
  benchmarkHaskellDepends = [
    base directory streamly-core tasty-bench
  ];
  homepage = "https://streamly.composewell.com";
  description = "Use OS processes as stream transformation functions";
  license = lib.licenses.asl20;
}
;
}
;
};
}