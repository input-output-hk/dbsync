import type {SidebarsConfig} from '@docusaurus/plugin-content-docs';

const sidebars: SidebarsConfig = {
  usersSidebar: [
    'intro',
    {
      type: 'category',
      label: 'Installation',
      collapsed: false,
      items: [
        'installation/prerequisites',
        'installation/linux',
        'installation/macos',
        'installation/building',
      ],
    },
    'node-setup',
    {
      type: 'category',
      label: 'Profiles',
      collapsed: false,
      items: [
        'profiles/overview',
        'profiles/presets',
        'profiles/custom',
      ],
    },
    'running',
    {
      type: 'category',
      label: 'Operations',
      collapsed: false,
      items: [
        'operations/metrics',
        'operations/troubleshooting',
        'operations/recovery',
      ],
    },
    'faq',
    'glossary',
  ],
};

export default sidebars;
