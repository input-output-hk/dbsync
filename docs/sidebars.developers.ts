import type {SidebarsConfig} from '@docusaurus/plugin-content-docs';

const sidebars: SidebarsConfig = {
  developersSidebar: [
    'intro',
    'architecture',
    {
      type: 'category',
      label: 'Phases',
      collapsed: false,
      items: [
        'phases/overview',
        'phases/ingest',
        'phases/preparing',
        'phases/following-volatile-tail',
        'phases/following-chain-tip',
      ],
    },
    'workers',
    {
      type: 'category',
      label: 'Extractors',
      collapsed: false,
      items: [
        'extractors/anatomy',
        'extractors/writing-one',
        'extractors/existing',
      ],
    },
    'schema-layer',
    'schema-versioning',
    'database-design',
    'memory-and-strictness',
    'testing',
    'repository-layout',
    'contributing',
  ],
};

export default sidebars;
