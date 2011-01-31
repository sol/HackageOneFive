CREATE TABLE package (name text PRIMARY KEY);
CREATE TABLE package_dependency (
    package_name text REFERENCES package,
    dependency_name text REFERENCES package,
    PRIMARY KEY (package_name, dependency_name));
