pub struct MiddleContext {
    pub current_location: Option<Location>,
    pub errors: Vec<MiddleErr>,
    pub stdlib_nodes: Vec<MiddleNode>,
    pub package_metadata: Option<PackageMetadata>,
}
