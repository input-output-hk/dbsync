import {themes as prismThemes} from 'prism-react-renderer';
import type {Config} from '@docusaurus/types';
import type * as Preset from '@docusaurus/preset-classic';

const config: Config = {
  title: 'dbsync',
  tagline: 'Fast, modular Cardano chain indexer',
  favicon: 'img/favicon.svg',

  url: 'https://input-output-hk.github.io',
  baseUrl: '/dbsync/',

  organizationName: 'input-output-hk',
  projectName: 'dbsync',
  trailingSlash: false,

  onBrokenLinks: 'throw',

  i18n: {
    defaultLocale: 'en',
    locales: ['en'],
  },

  markdown: {
    mermaid: true,
    hooks: {
      onBrokenMarkdownLinks: 'warn',
    },
  },

  themes: ['@docusaurus/theme-mermaid'],

  presets: [
    [
      'classic',
      {
        docs: false,
        blog: false,
        theme: {
          customCss: './src/css/custom.css',
        },
      } satisfies Preset.Options,
    ],
  ],

  plugins: [
    [
      '@docusaurus/plugin-content-docs',
      {
        id: 'users',
        path: 'users',
        routeBasePath: 'users',
        sidebarPath: './sidebars.users.ts',
        editUrl: 'https://github.com/input-output-hk/dbsync/edit/main/docs/',
      },
    ],
    [
      '@docusaurus/plugin-content-docs',
      {
        id: 'developers',
        path: 'developers',
        routeBasePath: 'developers',
        sidebarPath: './sidebars.developers.ts',
        editUrl: 'https://github.com/input-output-hk/dbsync/edit/main/docs/',
      },
    ],
    [
      require.resolve('@easyops-cn/docusaurus-search-local'),
      {
        hashed: true,
        indexBlog: false,
        docsDir: ['users', 'developers'],
        docsRouteBasePath: ['/users', '/developers'],
        docsPluginIdForPreferredVersion: 'users',
        highlightSearchTermsOnTargetPage: true,
      },
    ],
  ],

  themeConfig: {
    colorMode: {
      defaultMode: 'dark',
      respectPrefersColorScheme: true,
    },
    navbar: {
      title: 'dbsync',
      logo: {
        alt: 'dbsync',
        src: 'img/logo.svg',
      },
      items: [
        {
          type: 'docSidebar',
          docsPluginId: 'users',
          sidebarId: 'usersSidebar',
          position: 'left',
          label: 'Users',
        },
        {
          type: 'docSidebar',
          docsPluginId: 'developers',
          sidebarId: 'developersSidebar',
          position: 'left',
          label: 'Developers',
        },
        {
          href: 'https://github.com/input-output-hk/dbsync',
          position: 'right',
          className: 'header-github-link',
          'aria-label': 'GitHub repository',
        },
      ],
    },
    footer: {
      style: 'dark',
      links: [
        {
          title: 'Docs',
          items: [
            {label: 'Users', to: '/users/intro'},
            {label: 'Developers', to: '/developers/intro'},
          ],
        },
        {
          title: 'Project',
          items: [
            {label: 'GitHub', href: 'https://github.com/input-output-hk/dbsync'},
            {label: 'Issues', href: 'https://github.com/input-output-hk/dbsync/issues'},
          ],
        },
      ],
      copyright: `Copyright © ${new Date().getFullYear()} Input Output. Built with Docusaurus.`,
    },
    prism: {
      theme: prismThemes.github,
      darkTheme: prismThemes.dracula,
      additionalLanguages: ['haskell', 'bash', 'json', 'sql', 'yaml', 'toml', 'diff'],
    },
    mermaid: {
      theme: {light: 'neutral', dark: 'dark'},
    },
  } satisfies Preset.ThemeConfig,
};

export default config;
