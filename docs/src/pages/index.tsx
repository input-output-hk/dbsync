import type {ReactNode} from 'react';
import clsx from 'clsx';
import Link from '@docusaurus/Link';
import useDocusaurusContext from '@docusaurus/useDocusaurusContext';
import Layout from '@theme/Layout';
import Heading from '@theme/Heading';

import styles from './index.module.css';

type CardProps = {
  title: string;
  description: string;
  to: string;
  cta: string;
};

function Card({title, description, to, cta}: CardProps): ReactNode {
  return (
    <div className={clsx('col col--6', styles.card)}>
      <div className={styles.cardInner}>
        <Heading as="h2" className={styles.cardTitle}>
          {title}
        </Heading>
        <p className={styles.cardDescription}>{description}</p>
        <Link className="button button--primary button--lg" to={to}>
          {cta}
        </Link>
      </div>
    </div>
  );
}

function Hero(): ReactNode {
  const {siteConfig} = useDocusaurusContext();
  return (
    <header className={clsx('hero', styles.hero)}>
      <div className="container">
        <Heading as="h1" className={styles.heroTitle}>
          {siteConfig.title}
        </Heading>
        <p className={styles.heroTagline}>{siteConfig.tagline}</p>
      </div>
    </header>
  );
}

export default function Home(): ReactNode {
  const {siteConfig} = useDocusaurusContext();
  return (
    <Layout title={siteConfig.title} description={siteConfig.tagline}>
      <Hero />
      <main className={styles.main}>
        <div className="container">
          <div className="row">
            <Card
              title="Users"
              description="Install, configure with profiles, run, and operate dbsync against a Cardano node."
              to="/users/intro"
              cta="Read user docs"
            />
            <Card
              title="Developers"
              description="Architecture, phases, extractors, and contribution guide for working on dbsync itself."
              to="/developers/intro"
              cta="Read developer docs"
            />
          </div>
        </div>
      </main>
    </Layout>
  );
}
