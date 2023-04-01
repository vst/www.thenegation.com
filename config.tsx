import logo from '@/public/android-chrome-512x512.png';
import { faGithub, faLinkedin } from '@fortawesome/free-brands-svg-icons';
import { FontAwesomeIcon } from '@fortawesome/react-fontawesome';
import { SimpleLink } from './lib/website/types';

export interface SiteConfig {
  title: string;
  description: string;
  copyright: string;
  email: string;
  images: {
    avatar: any;
    about: any;
    gallery: {
      crooked: any[];
    };
  };
  linksNav: SimpleLink[];
  linksSocial: SimpleLink[];
}

const SiteConfig: SiteConfig = {
  title: 'the negation',
  description: 'as in the negation of negation...',
  copyright: `© ${new Date().getFullYear()} Vehbi Sinan Tunalioglu. All rights reserved.`,
  email: 'vst@vsthost.com',
  images: {
    avatar: logo,
    about: logo,
    gallery: {
      crooked: [
        'https://images.pexels.com/photos/2150/sky-space-dark-galaxy.jpg?cs=srgb&dl=pexels-pixabay-2150.jpg&fm=jpg&w=1280&h=919',
        'https://images.pexels.com/photos/2128249/pexels-photo-2128249.jpeg?cs=srgb&dl=pexels-g%C3%BCl-i%C5%9F%C4%B1k-2128249.jpg&fm=jpg&w=1280&h=1933',
        logo,
        'https://fastly.picsum.photos/id/403/800/800.jpg?hmac=kHKf_iLG1UpCJnG6bK03KPLleVYYdc3Qvwp2-dD_CEs',
        'https://upload.wikimedia.org/wikipedia/commons/1/1c/Haskell-Logo.svg',
      ],
    },
  },
  linksNav: [
    { name: 'Home', href: '/' },
    { name: 'Blog', href: '/posts/archive/1' },
    { name: 'About', href: '/about' },
  ],
  linksSocial: [
    {
      name: 'LinkedIn',
      href: 'https://www.linkedin.com/in/vehbisinan',
      icon: (props: any) => <FontAwesomeIcon icon={faLinkedin} {...props} />,
    },
    {
      name: 'GitHub',
      href: 'https://github.com/vst',
      icon: (props: any) => <FontAwesomeIcon icon={faGithub} {...props} />,
    },
  ],
};

export default SiteConfig;
